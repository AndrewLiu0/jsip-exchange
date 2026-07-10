(** End-to-end tests with a real server and RPC clients.

    These tests spin up an actual exchange server on a local port, connect
    one or more clients via RPC, log them in, and verify the full path:
    client -> network -> server -> matching engine -> dispatcher -> session
    feed -> client. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway
open Jsip_test_harness
open E2e_helpers

(* ---------------------------------------------------------------- *)
(* Multiple client tests *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: two clients trade with each other" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    (* Bob places a sell *)
    let%bind () = rpc_submit bob (Harness.sell ~price_cents:15000 ()) in
    [%expect {| [for Bob] ACCEPTED id=1 0 SELL 100@$150.00 DAY |}];
    (* Alice places a buy — should cross *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [for Alice] ACCEPTED id=2 0 BUY 100@$150.00 DAY
      [for Alice] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      [for Bob] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      |}];
    return ())
;;

let%expect_test "e2e: three clients, sequential orders, shared book" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind charlie = connect_as ~port Harness.charlie in
    (* Bob posts a sell *)
    let%bind () =
      rpc_submit bob (Harness.sell ~price_cents:15000 ~size:50 ())
    in
    [%expect {| [for Bob] ACCEPTED id=1 0 SELL 50@$150.00 DAY |}];
    (* Charlie posts a sell at a higher price *)
    let%bind () =
      rpc_submit charlie (Harness.sell ~price_cents:15010 ~size:50 ())
    in
    [%expect {| [for Charlie] ACCEPTED id=2 0 SELL 50@$150.10 DAY |}];
    (* Alice buys 80 — should sweep through both *)
    let%bind () =
      rpc_submit alice (Harness.buy ~price_cents:15010 ~size:80 ())
    in
    [%expect
      {|
      [for Alice] ACCEPTED id=3 0 BUY 80@$150.10 DAY
      [for Alice] FILL fill_id=1 0 $150.00 x50 aggressor=103|3(Alice) BUY resting=101|1(Bob)
      [for Alice] FILL fill_id=2 0 $150.10 x30 aggressor=103|3(Alice) BUY resting=102|2(Charlie)
      [for Bob] FILL fill_id=1 0 $150.00 x50 aggressor=103|3(Alice) BUY resting=101|1(Bob)
      [for Charlie] FILL fill_id=2 0 $150.10 x30 aggressor=103|3(Alice) BUY resting=102|2(Charlie)
      |}];
    (* Verify book state *)
    let%bind book = rpc_book alice Harness.aapl_id in
    print_endline (Option.value_exn book |> Book.to_string);
    [%expect
      {|
      === 0 ===
        BIDS: (empty)
        ASKS:
          $150.10 x20
        BBO: - / $150.10 x20
      |}];
    return ())
;;

let%expect_test "e2e: malformed symbol ids are rejected over the wire" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    (* Book query for an id the server doesn't trade: [None], same as an
       unknown symbol string used to be. *)
    let%bind book = rpc_book alice (Symbol_id.of_int_exn 999) in
    print_s [%sexp (book : Book.t option)];
    [%expect {| () |}];
    (* Submitting to an out-of-range id comes back asynchronously as an
       [Order_reject] on the session feed — the same contract as before. *)
    let%bind () =
      rpc_submit
        alice
        (Harness.buy
           ~price_cents:15000
           ~symbol:(Symbol_id.of_int_exn 999)
           ())
    in
    [%expect
      {| [for Alice] REJECTED 999 BUY 100@$150.00 reason=unknown symbol |}];
    return ())
;;

(* ---------------------------------------------------------------- *)
(* Market data subscription tests *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: market data subscriber receives trade and BBO updates" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind sub = connect_as ~port (Participant.of_string "Sub") in
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind result =
      Rpc.Pipe_rpc.dispatch
        Rpc_protocol.market_data_rpc
        (connection sub)
        [ Harness.aapl_id ]
    in
    let reader =
      match result with
      | Ok (Ok (reader, _id)) -> reader
      | _ -> failwith "subscribe failed"
    in
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         let e = Protocol.format_event event in
         print_endline [%string "[MD Subscriber] %{e}"]));
    (* Post a sell *)
    let%bind () = rpc_submit bob (Harness.sell ~price_cents:15000 ()) in
    [%expect
      {|
      [for Bob] ACCEPTED id=1 0 SELL 100@$150.00 DAY
      [MD Subscriber] BBO 0 bid=- ask=$150.00 x100
      |}];
    (* Cross it with a buy *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [for Alice] ACCEPTED id=2 0 BUY 100@$150.00 DAY
      [for Alice] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      [for Bob] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      [MD Subscriber] TRADE 0 $150.00 x100
      [MD Subscriber] BBO 0 bid=- ask=-
      |}];
    return ())
;;

let%expect_test "e2e: subscriber only sees events for subscribed symbol" =
  let gen = Client_order_id.Generator.create () in
  with_server ~symbols:[ Harness.aapl; Harness.tsla ] (fun ~server:_ ~port ->
    let%bind sub = connect_as ~port (Participant.of_string "Sub") in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind result =
      Rpc.Pipe_rpc.dispatch
        Rpc_protocol.market_data_rpc
        (connection sub)
        [ Harness.aapl_id ]
    in
    let reader =
      match result with
      | Ok (Ok (reader, _id)) -> reader
      | _ -> failwith "subscribe failed"
    in
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         let e = Protocol.format_event event in
         print_endline [%string "[MD Subscriber] %{e}"]));
    (* Post on TSLA — subscriber should NOT see this *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell
           ~price_cents:20000
           ~symbol:Harness.tsla_id
           ~client_order_id:(Client_order_id.Generator.generate gen)
           ())
    in
    [%expect {| [for Bob] ACCEPTED id=1 1 SELL 100@$200.00 DAY |}];
    (* Post on AAPL — subscriber SHOULD see this *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell
           ~price_cents:15000
           ~client_order_id:(Client_order_id.Generator.generate gen)
           ())
    in
    [%expect
      {|
      [for Bob] ACCEPTED id=2 0 SELL 100@$150.00 DAY
      [MD Subscriber] BBO 0 bid=- ask=$150.00 x100
      |}];
    return ())
;;

(* ---------------------------------------------------------------- *)
(* Concurrent submission test *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: many clients submit orders concurrently" =
  let gen = Client_order_id.Generator.create () in
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind seed = connect_as ~port Harness.bob in
    let%bind () =
      Deferred.List.iter
        (List.init 10 ~f:Fn.id)
        ~how:`Sequential
        ~f:(fun i ->
          rpc_submit
            seed
            (Harness.sell
               ~price_cents:(15000 + i)
               ~client_order_id:(Client_order_id.Generator.generate gen)
               ())
          |> Deferred.ignore_m)
    in
    let%bind () =
      Deferred.List.iter (List.init 5 ~f:Fn.id) ~how:`Parallel ~f:(fun i ->
        let participant = Participant.of_string [%string "Trader%{i#Int}"] in
        let%bind client = connect_as ~port participant in
        rpc_submit client (Harness.buy ~price_cents:15010 ())
        |> Deferred.ignore_m)
    in
    (* The dispatcher's placeholder [for <Participant>] prints land on stdout
       in an order that depends on which parallel buy was processed first.
       Swallow the trace and assert on the deterministic remaining book state
       instead: 10 sells went in, the 5 buys at $150.10 each hit the
       lowest-priced sell, so 5 sells should remain. *)
    let (_ : string) = [%expect.output] in
    let%bind book = rpc_book seed Harness.aapl_id in
    let book = Option.value_exn book in
    let remaining_orders = List.length book.bids + List.length book.asks in
    [%test_result: int] remaining_orders ~expect:5;
    return ())
;;

(* ---------------------------------------------------------------- *)
(* Audit log subscription tests *)
(* ---------------------------------------------------------------- *)

let%expect_test "e2e: audit log subscriber sees full unfiltered stream \
                 across symbols"
  =
  let gen = Client_order_id.Generator.create () in
  with_server ~symbols:[ Harness.aapl; Harness.tsla ] (fun ~server:_ ~port ->
    let%bind sub = connect_as ~port (Participant.of_string "Auditor") in
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    let%bind result =
      Rpc.Pipe_rpc.dispatch Rpc_protocol.audit_log_rpc (connection sub) ()
    in
    let reader =
      match result with
      | Ok (Ok (reader, _id)) -> reader
      | _ -> failwith "subscribe failed"
    in
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         let e = Protocol.format_event event in
         print_endline [%string "[AUDIT] %{e}"]));
    (* Post a sell on AAPL — audit subscriber should see ACCEPTED and BBO. *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell
           ~price_cents:15000
           ~client_order_id:(Client_order_id.Generator.generate gen)
           ())
    in
    [%expect
      {|
      [AUDIT] ACCEPTED id=1 0 SELL 100@$150.00 DAY
      [AUDIT] BBO 0 bid=- ask=$150.00 x100
      [for Bob] ACCEPTED id=1 0 SELL 100@$150.00 DAY
      |}];
    (* Post a sell on TSLA — audit subscriber should see this too
       (multi-symbol). *)
    let%bind () =
      rpc_submit
        bob
        (Harness.sell
           ~price_cents:20000
           ~symbol:Harness.tsla_id
           ~client_order_id:(Client_order_id.Generator.generate gen)
           ())
    in
    [%expect
      {|
      [AUDIT] ACCEPTED id=2 1 SELL 100@$200.00 DAY
      [AUDIT] BBO 1 bid=- ask=$200.00 x100
      [for Bob] ACCEPTED id=2 1 SELL 100@$200.00 DAY
      |}];
    (* Cross the AAPL sell — the audit log should see ACCEPTED + FILL + BBO. *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [AUDIT] ACCEPTED id=3 0 BUY 100@$150.00 DAY
      [AUDIT] FILL fill_id=1 0 $150.00 x100 aggressor=101|3(Alice) BUY resting=1|1(Bob)
      [AUDIT] TRADE 0 $150.00 x100
      [AUDIT] BBO 0 bid=- ask=-
      [for Alice] ACCEPTED id=3 0 BUY 100@$150.00 DAY
      [for Alice] FILL fill_id=1 0 $150.00 x100 aggressor=101|3(Alice) BUY resting=1|1(Bob)
      [for Bob] FILL fill_id=1 0 $150.00 x100 aggressor=101|3(Alice) BUY resting=1|1(Bob)
      |}];
    return ())
;;

let%expect_test "dispatcher: closing a subscriber's reader removes the \
                 writer"
  =
  let dispatcher =
    Dispatcher.create
      (Participant_id.Registry.create ())
      ~directory:(Symbol_directory.of_symbols [ Harness.aapl ])
  in
  print_s
    [%message
      "initial"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| (initial (count 0)) |}];
  let reader_a = Dispatcher.subscribe_audit dispatcher in
  let reader_b = Dispatcher.subscribe_audit dispatcher in
  print_s
    [%message
      "after subscribe"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| ("after subscribe" (count 2)) |}];
  Pipe.close_read reader_a;
  let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
  print_s
    [%message
      "after closing reader_a"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| ("after closing reader_a" (count 1)) |}];
  Pipe.close_read reader_b;
  let%bind () = Async.Scheduler.yield_until_no_jobs_remain () in
  print_s
    [%message
      "after closing reader_b"
        ~count:
          (Dispatcher.For_testing.audit_subscriber_count dispatcher : int)];
  [%expect {| ("after closing reader_b" (count 0)) |}];
  return ()
;;

(* ---------------------------------------------------------------- *)
(* RPC Cancel *)
(* ---------------------------------------------------------------- *)

let%expect_test "RPC Cancel: submit then cancel" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let client_order_id = Client_order_id.For_testing.of_int 1 in
    (* Bob places a sell *)
    let%bind () =
      rpc_submit alice (Harness.sell ~price_cents:15000 ~client_order_id ())
    in
    [%expect {| [for Alice] ACCEPTED id=1 0 SELL 100@$150.00 DAY |}];
    let%bind () = rpc_cancel alice client_order_id in
    [%expect
      {| [for Alice] CANCELLED id=1 0 remaining=100 reason=PARTICIPANT_REQUESTED |}];
    return ())
;;

let%expect_test "cancellation: duplicate client order ID " =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let client_order_id = Client_order_id.For_testing.of_int 1 in
    (* Bob places a sell *)
    let%bind () =
      rpc_submit alice (Harness.sell ~price_cents:15000 ~client_order_id ())
    in
    [%expect {| [for Alice] ACCEPTED id=1 0 SELL 100@$150.00 DAY |}];
    let%bind () =
      rpc_submit alice (Harness.sell ~price_cents:15000 ~client_order_id ())
    in
    [%expect
      {| [for Alice] REJECTED 0 SELL 100@$150.00 reason=duplicate client order id |}];
    return ())
;;

let%expect_test "e2e: cancel filled order error" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    (* Bob places a sell *)
    let%bind () = rpc_submit bob (Harness.sell ~price_cents:15000 ()) in
    [%expect {| [for Bob] ACCEPTED id=1 0 SELL 100@$150.00 DAY |}];
    (* Alice places a buy — should cross *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [for Alice] ACCEPTED id=2 0 BUY 100@$150.00 DAY
      [for Alice] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      [for Bob] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      |}];
    let%bind () = rpc_cancel alice (Client_order_id.For_testing.of_int 1) in
    [%expect
      {| [for Alice] REJECT CANCELLED client_id = 1 for participant Alice reason = order not found |}];
    return ())
;;

let%expect_test "RPC Cancel: cancel non-existent order" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let client_order_id = Client_order_id.For_testing.of_int 1 in
    (* Bob places a sell *)
    let%bind () =
      rpc_submit alice (Harness.sell ~price_cents:15000 ~client_order_id ())
    in
    [%expect {| [for Alice] ACCEPTED id=1 0 SELL 100@$150.00 DAY |}];
    let%bind () = rpc_cancel alice (Client_order_id.For_testing.of_int 4) in
    [%expect
      {| [for Alice] REJECT CANCELLED client_id = 4 for participant Alice reason = order not found |}];
    return ())
;;

let%expect_test "RPC Cancel: BBO Update" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let client_order_id = Client_order_id.For_testing.of_int 1 in
    (* Bob places a sell *)
    let%bind () =
      rpc_submit alice (Harness.sell ~price_cents:15000 ~client_order_id ())
    in
    [%expect {| [for Alice] ACCEPTED id=1 0 SELL 100@$150.00 DAY |}];
    let%bind book = rpc_book alice Harness.aapl_id in
    print_endline (Option.value_exn book |> Book.to_string);
    [%expect
      {|
      === 0 ===
        BIDS: (empty)
        ASKS:
          $150.00 x100
        BBO: - / $150.00 x100
      |}];
    let%bind () = rpc_cancel alice (Client_order_id.For_testing.of_int 1) in
    [%expect
      {| [for Alice] CANCELLED id=1 0 remaining=100 reason=PARTICIPANT_REQUESTED |}];
    let%bind book = rpc_book alice Harness.aapl_id in
    print_endline (Option.value_exn book |> Book.to_string);
    [%expect
      {|
      === 0 ===
        BIDS: (empty)
        ASKS: (empty)
        BBO: - / -
      |}];
    return ())
;;

let%expect_test "e2e: two login with same name" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    (* A second connection dispatches [login_rpc] directly (via [connect],
       which doesn't log in) so the rejection comes back as a value instead
       of an exception escaping [connect_as]. *)
    let%bind second = connect ~port in
    let%bind login_result =
      Rpc.Rpc.dispatch_exn
        Rpc_protocol.login_rpc
        (connection second)
        (Participant.to_string Harness.alice)
    in
    print_s [%sexp (login_result : Participant.t Or_error.t)];
    [%expect {| (Error "Participant already logged onto exchange") |}];
    (* Alice's original session is unaffected: it can still issue RPCs and
       receives events on its session feed. *)
    let%bind () = rpc_cancel alice (Client_order_id.For_testing.of_int 1) in
    [%expect
      {| [for Alice] REJECT CANCELLED client_id = 1 for participant Alice reason = order not found |}];
    return ())
;;

let%expect_test "e2e: subscribe session feed" =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind alice = connect_as ~port Harness.alice in
    let%bind bob = connect_as ~port Harness.bob in
    (* Subscribing session feed *)
    let%bind result =
      Rpc.Pipe_rpc.dispatch Rpc_protocol.session_feed_rpc (connection bob) ()
    in
    let reader =
      match result with
      | Ok (Ok (reader, _id)) -> reader
      | _ -> failwith "subscribe failed"
    in
    don't_wait_for
      (Pipe.iter_without_pushback reader ~f:(fun event ->
         let e = Protocol.format_event event in
         print_endline [%string "[for Bob] %{e}"]));
    (* Bob places a sell *)
    let%bind () = rpc_submit bob (Harness.sell ~price_cents:15000 ()) in
    [%expect {| [for Bob] ACCEPTED id=1 0 SELL 100@$150.00 DAY |}];
    (* Alice places a buy — should cross *)
    let%bind () = rpc_submit alice (Harness.buy ~price_cents:15000 ()) in
    [%expect
      {|
      [for Alice] ACCEPTED id=2 0 BUY 100@$150.00 DAY
      [for Alice] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      [for Bob] FILL fill_id=1 0 $150.00 x100 aggressor=102|2(Alice) BUY resting=101|1(Bob)
      |}];
    return ())
;;
