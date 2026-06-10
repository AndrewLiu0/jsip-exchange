open! Core
open! Async
open Jsip_gateway

let with_server ~symbols f =
  let%bind server = Exchange_server.start ~symbols ~port:0 () in
  let port = Exchange_server.port server in
  Monitor.protect
    (fun () -> f ~server ~port)
    ~finally:(fun () -> Exchange_server.close server)
;;

type client = { conn : Rpc.Connection.t }

let connect_as ~port _participant =
  let where =
    Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port }
  in
  let%map conn = Rpc.Connection.client where >>| Result.ok_exn in
  { conn }
;;

let connection client = client.conn

let rpc_submit client request =
  Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc client.conn request
  >>| ok_exn
;;

let rpc_book client symbol =
  Rpc.Rpc.dispatch_exn Rpc_protocol.book_query_rpc client.conn symbol
;;
