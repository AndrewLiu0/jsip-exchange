(** RPC definitions for the exchange's operational-statistics stream.

    Defined against [Async_rpc_kernel] rather than [Async] — they are the
    same [Rpc] module ([Async.Rpc] re-exports the kernel), but the kernel has
    no unix dependency, so this definition links into both the native server
    and the js_of_ocaml dashboard.

    This is deliberately a separate RPC from {!Jsip_gateway.Rpc_protocol}'s
    [audit_log_rpc]: the audit log records {e exchange events} (accepts,
    fills, cancels); this stream reports {e infrastructure metrics}.
    Conflating them would couple every audit subscriber to the metrics
    schema. *)

open! Core

(** Streams one {!Snapshot.t} per second (or per the server's configured
    stats period). Query is unit: there is nothing to filter — this is an
    operator/monitoring feed, like [audit_log_rpc]. *)
val stats_rpc : (unit, Snapshot.t, Error.t) Async_rpc_kernel.Rpc.Pipe_rpc.t
