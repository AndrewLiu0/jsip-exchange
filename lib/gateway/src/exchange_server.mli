(** Exchange server for production use and testing.

    Bundles the matching engine, market data bus, and RPC implementations
    into a single server that can be started on any port. Used by the server
    binary, the market maker binary, and integration tests. *)

open! Core
open! Async

type t

(** Start a server on the given port trading the symbols in [directory].
    Returns the server handle and the port it is actually listening on
    (useful when you pass port 0 to get an OS-assigned port).

    The caller builds the directory (e.g. [Symbol_directory.of_symbols] in
    the server binary's [main]) and it is authoritative for the whole run:
    the engine's books are indexed by the ids it assigned, and consumers
    mirror it to resolve names. It is passed in rather than built here —
    unlike the participant registry, which [start] creates itself — because
    the id assignment is public information that callers may need before the
    server exists (e.g. to describe scenarios).

    [stats_period] (default 1s) is how often the server publishes a
    {!Jsip_stats.Snapshot.t} on {!Jsip_stats.Stats_protocol.stats_rpc}; tests
    shrink it so they don't have to wait wall-clock seconds.

    [http_port], when given, opens a second listener that serves the same RPC
    implementations over websocket — the transport browsers can use (pass [0]
    for an OS-assigned port, readable via {!http_port}). Plain HTTP requests
    on that port go to [http_handler] (default: 501), which is how the server
    binary serves the web dashboard's static assets; the handler is injected
    so this library needs no knowledge of them. *)
val start
  :  ?stats_period:Time_ns.Span.t
  -> ?http_port:int
  -> ?http_handler:Rpc_websocket.Rpc.http_handler
  -> directory:Symbol_directory.t
  -> port:int
  -> unit
  -> t Deferred.t

(** The port the server is listening on. *)
val port : t -> int

(** The websocket/HTTP port, if [start] was given [http_port]. *)
val http_port : t -> int option

(** Stop the server and close all connections. *)
val close : t -> unit Deferred.t

(** Wait until the server's TCP listener is closed. *)
val close_finished : t -> unit Deferred.t
