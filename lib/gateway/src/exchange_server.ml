open! Core
open! Async
open Jsip_types
open Jsip_order_book

type request =
  | Submit of Order.Request.t
  | Cancel of Participant.t * Client_order_id.t

type t =
  { engine : Matching_engine.t
  ; dispatcher : Dispatcher.t
  ; request_writer : request Pipe.Writer.t
  ; tcp_server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
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

let handle_request ~request_writer (request : request) =
  let%map () = Pipe.write_if_open request_writer request in
  Ok ()
;;

let start_matching_loop
  ~engine
  ~dispatcher
  (request_reader : request Pipe.Reader.t)
  =
  don't_wait_for
    (Pipe.iter_without_pushback request_reader ~f:(fun request ->
       match request with
       | Submit submit_request ->
         let events = Matching_engine.submit engine submit_request in
         Dispatcher.dispatch dispatcher events
       | Cancel (participant, client_order_id) -> 
        let events = Matching_engine.cancel engine participant client_order_id in 
        Dispatcher.dispatch dispatcher events ))
;;

let start ~symbols ~port () =
  let engine = Matching_engine.create symbols in
  let dispatcher = Dispatcher.create () in
  (* request_writer: network RPC handlers write incoming client requests here
     request_reader: the backend exexution engine consumes requests from this
     stream
  *)
  let request_reader, request_writer = Pipe.create () in
  Pipe.set_size_budget request_writer request_queue_size_budget;
  start_matching_loop ~engine ~dispatcher request_reader;
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
                 let new_request = { request with participant } in
                 handle_request ~request_writer (Submit new_request)
               | None -> return (Or_error.error_string "Not logged_in"))
        ; Rpc.Rpc.implement
            Rpc_protocol.cancel_order_rpc
            (fun state client_order_id ->
               match Connection_state.participant state with
               | Some participant ->
                handle_request ~request_writer (Cancel (participant, client_order_id) )
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
        ]
      ~on_unknown_rpc:`Close_connection
      ~on_exception:Log_on_background_exn
  in
  let%map tcp_server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _addr conn ->
        let state = { Connection_state.session = None } in
        don't_wait_for
          (let%bind () = Rpc.Connection.close_finished conn in
           match Connection_state.participant state with
           | Some _participant ->
             (match state.session with
              | None -> return ()
              | Some session ->
                Dispatcher.clean_up_session dispatcher session)
           | None -> return ());
        state)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  let actual_port = Tcp.Server.listening_on tcp_server in
  { engine; dispatcher; request_writer; tcp_server; port = actual_port }
;;

let port t = t.port

let close t =
  Pipe.close t.request_writer;
  Tcp.Server.close t.tcp_server
;;

let close_finished t = Tcp.Server.close_finished t.tcp_server
