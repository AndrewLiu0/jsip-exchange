open! Core
open! Async
open Jsip_types
open Jsip_order_book
open Jsip_stats

type action =
  | Submit of Participant.t * Order.Request.t
  | Cancel of Participant.t * Client_order_id.t

(* [received_at] is stamped when the RPC handler enqueues the action and read
   again when the matching loop has handled it; the difference is the
   submit/cancel latency reported on the stats stream. It must travel with
   the request: the RPC response resolves on enqueue, so time spent queued
   behind other requests is only observable from inside the queue. *)
type request =
  { received_at : Time_ns.t
  ; action : action
  }

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : request Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  ; stats_recorder : Stats_recorder.t
  ; http_server : (Socket.Address.Inet.t, int) Cohttp_async.Server.t option
  }

module Connection_state = struct
  type t = { mutable session : Session.t option }

  let participant t = Option.map t.session ~f:Session.participant
end

(* Bound how many client requests can sit in the queue waiting for the
   matching engine. Once the queue is full, [Pipe.write] returns a pending
   deferred and the [submit_order_rpc] handler blocks until the engine has
   processed enough requests to free up space — clients get backpressure
   without the server's memory growing unboundedly. *)
let request_queue_size_budget = 1024

let handle_request ~request_writer (action : action) =
  let request = { received_at = Time_ns.now (); action } in
  let%map () = Pipe.write_if_open request_writer request in
  Ok ()
;;

let start_matching_loop
  ~engine
  ~dispatcher
  ~stats_recorder
  (request_reader : request Pipe.Reader.t)
  =
  don't_wait_for
    (Pipe.iter_without_pushback
       request_reader
       ~f:(fun { received_at; action } ->
         let elapsed_since_received () =
           Time_ns.diff (Time_ns.now ()) received_at
         in
         match action with
         | Submit (participant, submit_request) ->
           let events =
             Matching_engine.submit engine ~participant submit_request
           in
           Dispatcher.dispatch dispatcher events;
           Stats_recorder.record_submit_latency
             stats_recorder
             (elapsed_since_received ())
         | Cancel (participant, client_order_id) ->
           let events =
             Matching_engine.cancel engine participant client_order_id
           in
           Dispatcher.dispatch dispatcher events;
           Stats_recorder.record_cancel_latency
             stats_recorder
             (elapsed_since_received ())))
;;

let build_snapshot ~dispatcher ~stats_recorder ~request_writer : Snapshot.t =
  (* [Gc.stat] rather than [Gc.quick_stat]: only [stat] reports [live_words].
     It walks the heap to compute it — fine at the stats cadence, not
     something to call in a hot loop. *)
  { time = Time_ns.now ()
  ; memory = Snapshot.Memory.of_gc_stat (Gc.stat ())
  ; submit_latency = Stats_recorder.take_submit_latency stats_recorder
  ; cancel_latency = Stats_recorder.take_cancel_latency stats_recorder
  ; pipe_occupancy =
      { request_queue = Pipe.length request_writer
      ; market_data = Dispatcher.market_data_queue_lengths dispatcher
      ; audit = Dispatcher.audit_queue_lengths dispatcher
      ; sessions = Dispatcher.session_queue_lengths dispatcher
      }
  }
;;

(* Shared by the TCP and websocket transports so the session-cleanup logic
   cannot drift between them: whatever transport a connection arrived on,
   closing it releases the participant's session. *)
let create_connection_state ~dispatcher conn =
  let state = { Connection_state.session = None } in
  don't_wait_for
    (let%bind () = Rpc.Connection.close_finished conn in
     match state.session with
     | None -> return ()
     | Some session -> Dispatcher.clean_up_session dispatcher session);
  state
;;

let default_stats_period = Time_ns.Span.of_sec 1.0

let start
  ?(stats_period = default_stats_period)
  ?http_port
  ?http_handler
  ~symbols
  ~port
  ()
  =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  let stats_recorder = Stats_recorder.create () in
  (* request_writer: network RPC handlers write incoming client requests here
     request_reader: the backend exexution engine consumes requests from this
     stream
  *)
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher ~stats_recorder request_reader;
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement
            Rpc_protocol.login_rpc
            (fun (state : Connection_state.t) request ->
               let trimmed = String.strip request in
               if String.is_empty trimmed
               then return (Or_error.error_string "Invalid participant name")
               else (
                 let participant = Participant.of_string trimmed in
                 let session = Session.create participant in
                 match Dispatcher.register_session dispatcher session with
                 | Ok () ->
                   state.session <- Some session;
                   return (Ok participant)
                 | Error err -> return (Error err)))
        ; Rpc.Rpc.implement
            Rpc_protocol.submit_order_rpc
            (fun state request ->
               match Connection_state.participant state with
               | Some participant ->
                 handle_request
                   ~request_writer
                   (Submit (participant, request))
               | None -> return (Or_error.error_string "Not logged_in"))
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun state client_order_id ->
               match Connection_state.participant state with
               | Some participant ->
                 handle_request
                   ~request_writer
                   (Cancel (participant, client_order_id))
               | None -> return (Or_error.error_string "Not logged_in"))
        ; Rpc.Rpc.implement' Rpc_protocol.book_query_rpc (fun state symbol ->
            ignore state;
            Matching_engine.book engine symbol
            |> Option.map ~f:Order_book.snapshot)
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.market_data_rpc
            (fun state symbols ->
               ignore state;
               let reader =
                 Dispatcher.subscribe_market_data dispatcher symbols
               in
               return (Ok reader))
        ; Rpc.Pipe_rpc.implement Rpc_protocol.audit_log_rpc (fun state () ->
            ignore state;
            let reader = Dispatcher.subscribe_audit dispatcher in
            return (Ok reader))
        ; Rpc.Pipe_rpc.implement
            Rpc_protocol.session_feed_rpc
            (fun (state : Connection_state.t) () ->
               match state.session with
               | None -> return (Error (Error.of_string "No session exists"))
               | Some session -> return (Ok (Session.reader session)))
        ; Rpc.Pipe_rpc.implement
            Stats_protocol.stats_rpc
            (fun (_ : Connection_state.t) () ->
               return (Ok (Stats_recorder.subscribe stats_recorder)))
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%bind tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr conn ->
        create_connection_state ~dispatcher conn)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  (* The same [implementations] served over a second, websocket transport:
     browsers cannot open raw TCP connections, so this is how the web
     dashboard reaches the exchange. Plain (non-websocket) HTTP requests on
     the same port go to [http_handler] — the server binary uses that to
     serve the dashboard's HTML and JavaScript. *)
  let%map http_server =
    match http_port with
    | None -> return None
    | Some http_port ->
      let%map http_server =
        Rpc_websocket.Rpc.serve
          ~where_to_listen:(Tcp.Where_to_listen.of_port http_port)
          ~implementations
          ~initial_connection_state:
            (fun
              ()
              (_ : Rpc_websocket.Rpc.Connection_initiated_from.t)
              _addr
              conn
            -> create_connection_state ~dispatcher conn)
          ?http_handler:
            (Option.map http_handler ~f:(fun handler () -> handler))
          ~on_handler_error:`Ignore
          ()
      in
      Some http_server
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  Clock_ns.every
    ~stop:(Tcp.Server.close_finished tcp_server)
    stats_period
    (fun () ->
       Stats_recorder.publish
         stats_recorder
         (build_snapshot ~dispatcher ~stats_recorder ~request_writer));
  { engine
  ; dispatcher
  ; request_writer
  ; tcp_server
  ; port = actual_port
  ; stats_recorder
  ; http_server
  }
;;

let port t = t.port

let http_port t =
  Option.map t.http_server ~f:Cohttp_async.Server.listening_on
;;

let close t =
  Pipe.close t.request_writer;
  let%bind () =
    match t.http_server with
    | None -> return ()
    | Some http_server -> Cohttp_async.Server.close http_server
  in
  Tcp.Server.close t.tcp_server
;;

let close_finished t =
  let http_closed =
    match t.http_server with
    | None -> return ()
    | Some http_server -> Cohttp_async.Server.close_finished http_server
  in
  Deferred.all_unit [ Tcp.Server.close_finished t.tcp_server; http_closed ]
;;
