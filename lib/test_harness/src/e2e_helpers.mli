(** Shared helpers for end-to-end tests that use a real server and RPC
    clients. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway

(** Start a server on an OS-assigned port, run [f], then shut down. *)
val with_server
  :  symbols:Symbol.t list
  -> (server:Exchange_server.t -> port:int -> 'a Deferred.t)
  -> 'a Deferred.t

(** A test client: an open RPC connection to the server. A future revision
    (once the session-feed RPC and login flow exist) will extend this with a
    buffered session feed so [rpc_submit] can return the events produced by
    the just-submitted request. *)
type client

(** Connect a client to [port]. The [participant] argument is used to subscribe
to session_feed_rpc that printss every event it receives with a participant tag prefix *)
val connect_as : port:int -> Participant.t -> client Deferred.t

(** The raw RPC connection, useful for tests that exercise unusual RPC paths
    (audit log subscriptions, second clients on the same connection, etc.). *)
val connection : client -> Rpc.Connection.t

(** Submit an order via RPC. The RPC is one-way: this returns once the server
    has enqueued the request. Participant-targeted events (acceptance, fills,
    rejection) are currently printed on the server's stdout via the
    dispatcher's session stub. *)
val rpc_submit : client -> Order.Request.t -> unit Deferred.t

(** Query the book via RPC. *)
val rpc_book : client -> Symbol.t -> Book.t option Deferred.t
