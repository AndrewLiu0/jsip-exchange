(** Exchange server.

    Runs the matching engine and listens for RPC connections from clients..

    Run with: dune exec app/server/bin/main.exe -- -port 12345

    Optionally generate demo traffic for the monitor: dune exec
    app/server/bin/main.exe -- -port 12345 -trade-back-and-forth

    For a realistic market with the dynamic {!Market_maker_bot} and other
    bots, use the scenario runner instead of this binary directly. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway
open Jsip_market_maker

let default_symbols =
  [ Symbol.of_string "AAPL"
  ; Symbol.of_string "TSLA"
  ; Symbol.of_string "GOOG"
  ; Symbol.of_string "MSFT"
  ]
;;

let connect_as ~where_to_connect participant =
  let%bind conn = Rpc.Connection.client where_to_connect >>| Result.ok_exn in
  let%map (_ : Participant.t) =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      conn
      (Participant.to_string participant)
    >>| ok_exn
  in
  conn
;;

(* Two market makers per symbol with offset fair values: MM_High's bids cross
   MM_Low's asks every cycle, producing a steady stream of [Fill] /
   [Trade_report] events across multiple symbols for the monitor to render.

   This mode leaks by design and is for short demos, not long-running load
   tests: [Market_maker.seed_book] always submits Day orders and never
   cancels, so the un-crossable levels (MM_Low's bids and MM_High's asks)
   accumulate in the book every cycle; and since neither participant reads
   its session feed, the accepts and fills also accumulate unread in the
   session pipes. *)
let trade_back_and_forth ~where_to_connect =
  (* One pair of MMs per symbol, anchored at a representative fair value. *)
  let symbol_anchors =
    [ Symbol.of_string "AAPL", 15000
    ; Symbol.of_string "TSLA", 25000
    ; Symbol.of_string "GOOG", 28000
    ]
  in
  (* MM_Low's fair value sits [low_offset_cents] below the anchor and
     MM_High's sits [high_offset_cents] above. The offsets are asymmetric so
     MM_High's bid (at [anchor + high_offset_cents - half_spread]) crosses
     MM_Low's ask (at [anchor + low_offset_cents + half_spread]). *)
  let low_offset_cents = -10 in
  let high_offset_cents = 15 in
  let cycle_period = Time_ns.Span.of_sec 2. in
  let make ~participant ~symbol ~fair_value_cents : Market_maker.Config.t =
    { participant
    ; symbol
    ; fair_value_cents
    ; half_spread_cents = 5
    ; size_per_level = 25
    ; num_levels = 3
    }
  in
  (* Two market makers total, each shared across all symbols — so we open
     exactly one logged-in connection per participant. *)
  let mm_low = Participant.of_string "MM_Low" in
  let mm_high = Participant.of_string "MM_High" in
  let%bind low_conn = connect_as ~where_to_connect mm_low in
  let%bind high_conn = connect_as ~where_to_connect mm_high in
  (* One generator per participant, shared across every symbol and cycle: the
     exchange never forgets a used client_order_id, so restarting a generator
     would get all later submissions rejected as duplicates. *)
  let low_ids = Client_order_id.Generator.create () in
  let high_ids = Client_order_id.Generator.create () in
  let configs_for_symbol (symbol, anchor) =
    [ ( low_conn
      , low_ids
      , make
          ~participant:mm_low
          ~symbol
          ~fair_value_cents:(anchor + low_offset_cents) )
    ; ( high_conn
      , high_ids
      , make
          ~participant:mm_high
          ~symbol
          ~fair_value_cents:(anchor + high_offset_cents) )
    ]
  in
  let configs = List.concat_map symbol_anchors ~f:configs_for_symbol in
  let cycle () =
    Deferred.List.iter
      ~how:`Sequential
      configs
      ~f:(fun (conn, ids, config) -> Market_maker.seed_book config conn ~ids)
  in
  let%map () = cycle () in
  Clock_ns.every cycle_period (fun () -> don't_wait_for (cycle ()))
;;

let start ~port ~http_port ~market_maker_behavior =
  let%bind server =
    Exchange_server.start
      ?http_port
      ~http_handler:Jsip_dashboard_assets.handler
      ~symbols:default_symbols
      ~port
      ()
  in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%bind () =
    match market_maker_behavior with
    | `Trade_back_and_forth ->
      let%map () =
        print_endline
          "=== Starting two market makers trading back-and-forth ===";
        trade_back_and_forth ~where_to_connect
      in
      print_endline ""
    | `Do_nothing -> Deferred.unit
  in
  print_endline
    [%string
      "JSIP Exchange server listening on port %{Exchange_server.port \
       server#Int}"];
  (match Exchange_server.http_port server with
   | None -> ()
   | Some http_port ->
     print_endline [%string "Dashboard: http://localhost:%{http_port#Int}"]);
  let symbols =
    List.map default_symbols ~f:Symbol.to_string |> String.concat ~sep:", "
  in
  print_endline [%string "Trading: %{symbols}"];
  Exchange_server.close_finished server
;;

let () =
  Command.async
    ~summary:"JSIP Exchange server"
    (let%map_open.Command port =
       flag "-port" (required int) ~doc:"PORT port to listen on"
     and http_port =
       flag
         "-http-port"
         (optional int)
         ~doc:
           "PORT also serve the browser dashboard (and RPCs over websocket) \
            on this port"
     and market_maker_behavior =
       choose_one
         ~if_nothing_chosen:(Default_to `Do_nothing)
         [ flag
             "-trade-back-and-forth"
             (no_arg_some `Trade_back_and_forth)
             ~doc:
               " run two market makers in a loop, generating sustained \
                traffic for the monitor"
         ]
     in
     fun () -> start ~port ~http_port ~market_maker_behavior)
  |> Command_unix.run
;;
