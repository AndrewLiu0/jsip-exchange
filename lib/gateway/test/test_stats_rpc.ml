(** End-to-end test of the [exchange-stats] pipe RPC: boot a real server
    (with a fast stats period so the test doesn't wait wall-clock seconds),
    generate one submit and one cancel, and check the stream reports them.

    Raw snapshot values (times, latencies, memory) are nondeterministic, so
    the test prints a normalized view: counts, sign checks, and the
    pipe-occupancy fields that are deterministic. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway
open Jsip_stats
open Jsip_test_harness

(* Read snapshots until, cumulatively, both one submit and one cancel have
   been reported; return the snapshot that completed the picture. *)
let rec read_totals reader ~submit_total ~cancel_total =
  match%bind Pipe.read reader with
  | `Eof -> failwith "stats pipe closed before reporting activity"
  | `Ok (snapshot : Snapshot.t) ->
    let submit_total = submit_total + snapshot.submit_latency.total_count in
    let cancel_total = cancel_total + snapshot.cancel_latency.total_count in
    (match submit_total >= 1 && cancel_total >= 1 with
     | true -> return (snapshot, submit_total, cancel_total)
     | false -> read_totals reader ~submit_total ~cancel_total)
;;

let%expect_test "exchange-stats reflects submits, cancels, and sessions" =
  Harness.reset_client_order_id_counter ();
  let%bind server =
    Exchange_server.start
      ~stats_period:(Time_ns.Span.of_ms 50.)
      ~symbols:[ Harness.aapl ]
      ~port:0
      ()
  in
  let port = Exchange_server.port server in
  Monitor.protect
    ~finally:(fun () -> Exchange_server.close server)
    (fun () ->
      (* Log in without subscribing to the session feed: the feed's events
         stay buffered, which is exactly what the [sessions] occupancy below
         counts. *)
      let%bind client = E2e_helpers.connect ~port in
      let conn = E2e_helpers.connection client in
      let%bind login_result =
        Rpc.Rpc.dispatch_exn Rpc_protocol.login_rpc conn "Alice"
      in
      let (_ : Participant.t) = Or_error.ok_exn login_result in
      let%bind snapshots, (_ : Rpc.Pipe_rpc.Metadata.t) =
        Rpc.Pipe_rpc.dispatch_exn Stats_protocol.stats_rpc conn ()
      in
      (* One resting buy (-> Order_accept to Alice's session) and one cancel
         of an unknown id (-> Cancel_reject to Alice's session). *)
      let%bind () =
        E2e_helpers.rpc_submit client (Harness.buy ~price_cents:10_000 ())
      in
      let%bind () =
        E2e_helpers.rpc_cancel
          client
          (Client_order_id.For_testing.of_int 999)
      in
      let%bind final, submit_total, cancel_total =
        read_totals snapshots ~submit_total:0 ~cancel_total:0
      in
      let all_samples =
        Array.append
          final.submit_latency.samples
          final.cancel_latency.samples
      in
      print_s
        [%message
          ""
            (submit_total : int)
            (cancel_total : int)
            ~samples_are_nonnegative:
              (Array.for_all all_samples ~f:(fun span ->
                 Time_ns.Span.( >= ) span Time_ns.Span.zero)
               : bool)
            ~live_words_is_positive:(final.memory.live_words > 0 : bool)
            ~request_queue:(final.pipe_occupancy.request_queue : int)
            ~market_data:
              (final.pipe_occupancy.market_data : (Symbol.t * int list) list)
            ~audit:(final.pipe_occupancy.audit : int list)
            ~sessions:
              (final.pipe_occupancy.sessions : (Participant.t * int) list)];
      [%expect
        {|
         ((submit_total 1) (cancel_total 1) (samples_are_nonnegative true)
          (live_words_is_positive true) (request_queue 0) (market_data ()) (audit ())
          (sessions ((Alice 2))))
         |}];
      return ())
;;

let%expect_test "exchange-stats is reachable over websocket" =
  let%bind server =
    Exchange_server.start
      ~stats_period:(Time_ns.Span.of_ms 50.)
      ~http_port:0
      ~symbols:[ Harness.aapl ]
      ~port:0
      ()
  in
  Monitor.protect
    ~finally:(fun () -> Exchange_server.close server)
    (fun () ->
      let http_port = Option.value_exn (Exchange_server.http_port server) in
      let%bind conn =
        Rpc_websocket.Rpc.client
          (Uri.of_string [%string "ws://localhost:%{http_port#Int}/"])
        >>| ok_exn
      in
      let%bind snapshots, (_ : Rpc.Pipe_rpc.Metadata.t) =
        Rpc.Pipe_rpc.dispatch_exn Stats_protocol.stats_rpc conn ()
      in
      match%bind Pipe.read snapshots with
      | `Eof -> failwith "stats pipe closed before the first snapshot"
      | `Ok (snapshot : Snapshot.t) ->
        print_s
          [%message
            "snapshot received over websocket"
              ~live_words_is_positive:(snapshot.memory.live_words > 0 : bool)];
        [%expect
          {| ("snapshot received over websocket" (live_words_is_positive true)) |}];
        return ())
;;
