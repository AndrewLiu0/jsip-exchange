(** Server-side accumulator behind {!Jsip_stats.Stats_protocol.stats_rpc}.

    Sits between the matching loop (which records one latency sample per
    handled request) and the once-per-period snapshot loop in
    {!Exchange_server} (which drains the accumulated samples into a
    {!Jsip_stats.Snapshot.t} and publishes it to every subscriber).

    Samples are capped at [max_samples_per_kind] per snapshot interval so a
    request flood cannot grow a snapshot without bound; the true number of
    observations is still reported via
    {!Jsip_stats.Snapshot.Latency.total_count}. *)

open! Core
open! Async
open Jsip_stats

type t

val create : ?max_samples_per_kind:int (** default 1,000 *) -> unit -> t

(** Called by the matching loop once per handled request. O(1); past the cap
    only the counter moves. *)
val record_submit_latency : t -> Time_ns.Span.t -> unit

val record_cancel_latency : t -> Time_ns.Span.t -> unit

(** Everything recorded since the previous [take_*] call, resetting the
    accumulator. Called once per snapshot interval. *)
val take_submit_latency : t -> Snapshot.Latency.t

val take_cancel_latency : t -> Snapshot.Latency.t

(** Register a stats subscriber. Same lifecycle as
    {!Dispatcher.subscribe_audit}: the pipe is dropped from the registry when
    the reader is closed. *)
val subscribe : t -> Snapshot.t Pipe.Reader.t

(** Push one snapshot to every subscriber (without pushback — snapshots are
    small and periodic, and a slow stats consumer must not stall the
    exchange). *)
val publish : t -> Snapshot.t -> unit
