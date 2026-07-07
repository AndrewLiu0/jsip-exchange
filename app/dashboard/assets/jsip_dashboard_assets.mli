(** The browser dashboard's assets, embedded at build time, and the HTTP
    handler that serves them.

    Any server binary that passes [~http_port] to
    {!Jsip_gateway.Exchange_server.start} can hand this handler along as
    [~http_handler] and browsers pointed at that port get the dashboard;
    websocket upgrades on the same port never reach it (they become RPC
    connections inside the gateway). Both the exchange server binary and the
    scenario runner use it, e.g.:

    {[
      Exchange_server.start
        ?http_port
        ~http_handler:Jsip_dashboard_assets.handler
        ~symbols
        ~port
        ()
    ]} *)

open! Core
open! Async

(** Serves [/] (and [/index.html]) and [/main.bc.js] from the embedded
    copies; anything else is a 404. The type is
    [Rpc_websocket.Rpc.http_handler], spelled out so this library doesn't
    depend on the websocket library. *)
val handler
  :  body:Cohttp_async.Body.t
  -> Socket.Address.Inet.t
  -> Cohttp_async.Request.t
  -> Cohttp_async.Server.response Deferred.t
