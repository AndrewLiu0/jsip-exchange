open! Core
open! Async
open Jsip_types
open Jsip_gateway
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module News_injector = Jsip_news_injector.News_injector
module Bot_runtime = Jsip_bot_runtime.Bot_runtime

(* Bring up one bot end-to-end: open its own RPC connection, subscribe to the
   market-data stream for the symbols listed in the spec, and run the bot.
   Once the session feed exists (week 2 exercise 1) this is also where each
   bot will log in and subscribe to its session-feed RPC, so its [on_event]
   handler can react to the matching engine's responses to its own orders and
   to fills against its resting orders. *)
let start_bot ~where_to_connect ~oracle ~directory (Bot_spec.T spec) =
  let%bind connection =
    Rpc.Connection.client where_to_connect
    >>| Result.map_error ~f:Error.of_exn
    >>| ok_exn
  in
  let submit request =
    Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc connection request
  in
  let cancel (client_order_id : Client_order_id.t) =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.cancel_order_rpc
      connection
      client_order_id
  in
  let%bind login_result =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      connection
      (Participant.to_string spec.participant)
  in
  let (_ : Participant.t) = Or_error.ok_exn login_result in
  (* Subscribing to the session feed *)
  let bot =
    Bot_runtime.create
      spec.bot
      spec.config
      ~participant:spec.participant
      ~oracle
      ~rng:(Splittable_random.of_int spec.rng_seed)
      ~submit
      ~cancel
      ~symbol_id:(Symbol_directory.id directory)
      ~symbol_name:(Symbol_directory.name directory)
      ~tick_interval:spec.tick_interval
  in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc connection ()
  in
  don't_wait_for (Pipe.iter session_feed ~f:(Bot_runtime.feed_event bot));
  let%bind () =
    match spec.is_marketdata_consumer with
    | false -> return ()
    | true ->
      (* Scenario specs name symbols; the wire wants ids. A name the exchange
         doesn't trade is a scenario wiring bug, so fail loudly rather than
         silently subscribing to nothing. *)
      let symbol_ids =
        List.map spec.symbols ~f:(fun symbol ->
          match Symbol_directory.id directory symbol with
          | Some id -> id
          | None ->
            raise_s
              [%message
                "scenario subscribes to a symbol the exchange doesn't trade"
                  (symbol : Symbol.t)])
      in
      let%bind md_pipe, metadata =
        Rpc.Pipe_rpc.dispatch_exn
          Rpc_protocol.market_data_rpc
          connection
          symbol_ids
      in
      don't_wait_for
        (let%bind () = Pipe.iter md_pipe ~f:(Bot_runtime.feed_event bot) in
         match%map Rpc.Pipe_rpc.close_reason metadata with
         | Rpc.Pipe_close_reason.Closed_locally
         | Rpc.Pipe_close_reason.Closed_remotely ->
           ()
         | Rpc.Pipe_close_reason.Error err ->
           [%log.error "marketdata pipe closed with error" (err : Error.t)]);
      return ()
  in
  print_endline
    [%string "[scenario] starting bot %{spec.participant#Participant}"];
  don't_wait_for (Bot_runtime.start bot);
  return ()
;;

let run ?http_port (config : Scenario_config.t) ~port ~seed =
  print_endline
    [%string
      "[scenario] starting %{config.name} on port %{port#Int} \
       (seed=%{seed#Int})"];
  (* The authoritative id assignment for this run: symbol i in the scenario's
     list gets id i. Built here (not inside the server) so the runner can
     hand bots their name<->id mirror before any of them starts. *)
  let directory = Symbol_directory.of_symbols config.symbols in
  let%bind server =
    Exchange_server.start
      ?http_port
      ~http_handler:Jsip_dashboard_assets.handler
      ~directory
      ~port
      ()
  in
  (match Exchange_server.http_port server with
   | None -> ()
   | Some http_port ->
     print_endline
       [%string "[scenario] dashboard: http://localhost:%{http_port#Int}"]);
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port
      { Host_and_port.host = "localhost"; port }
  in
  let oracle = Fundamental_oracle.create config.oracle_config ~seed in
  let injector = News_injector.create oracle config.news in
  (* Background tasks. *)
  don't_wait_for (Fundamental_oracle.start oracle);
  don't_wait_for (News_injector.start injector);
  let%bind () =
    Deferred.List.iter
      ~how:`Parallel
      config.bots
      ~f:(start_bot ~where_to_connect ~oracle ~directory)
  in
  Exchange_server.close_finished server
;;
