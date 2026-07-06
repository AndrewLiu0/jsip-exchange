open! Core
open! Async
open Jsip_gateway
open Jsip_types

let with_server ~symbols f =
  (* Without this reset, default client order IDs would depend on expect-test
     execution order. *)
  Harness.reset_client_order_id_counter ();
  let%bind server = Exchange_server.start ~symbols ~port:0 () in
  let port = Exchange_server.port server in
  Monitor.protect
    (fun () -> f ~server ~port)
    ~finally:(fun () -> Exchange_server.close server)
;;

type client = { conn : Rpc.Connection.t }

let connect ~port =
  let where =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%map conn = Rpc.Connection.client where >>| Result.ok_exn in
  { conn }
;;

let connect_as ~port (participant : Participant.t) =
  let%bind { conn } = connect ~port in
  let%bind login_result =
    Rpc.Rpc.dispatch_exn
      Rpc_protocol.login_rpc
      conn
      (Participant.to_string participant)
  in
  let (_ : Participant.t) = Or_error.ok_exn login_result in
  let%bind session_feed, _metadata =
    Rpc.Pipe_rpc.dispatch_exn Rpc_protocol.session_feed_rpc conn ()
  in
  don't_wait_for
    (Pipe.iter_without_pushback session_feed ~f:(fun event ->
       let e = Protocol.format_event event in
       print_endline [%string "[for %{participant#Participant}] %{e}"]));
  return { conn }
;;

let connection client = client.conn

let rpc_submit client request =
  Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc client.conn request
  >>| ok_exn
;;

let rpc_book client symbol =
  Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc client.conn symbol
;;

let rpc_cancel client client_order_id =
  Rpc.Rpc.dispatch_exn
    Rpc_protocol.cancel_order_rpc
    client.conn
    client_order_id
  >>| ok_exn
;;
