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
open Jsip_types
open Jsip_stats

type t

val create : ?max_samples_per_kind:int (** default 1,000 *) -> unit -> t

(** Called by the matching loop once per handled request. Records the latency
    sample and bumps [participant]'s per-interval activity counter (see
    {!Jsip_stats.Snapshot.Participant_activity}). O(1); past the sample cap
    only the counters move. *)
val record_submit_latency
  :  t
  -> participant:Participant.t
  -> Time_ns.Span.t
  -> unit

val record_cancel_latency
  :  t
  -> participant:Participant.t
  -> Time_ns.Span.t
  -> unit

(** Called by the matching loop with the events each handled request
    produced. Counts every [Order_reject], [Cancel_reject], and
    [Order_cancel] by its reason (see {!Jsip_stats.Snapshot.Reject_counts});
    other events are ignored. *)
val record_events : t -> Exchange_event.t list -> unit

(** Everything recorded since the previous [take_*] call, resetting the
    accumulator. Called once per snapshot interval. *)
val take_submit_latency : t -> Snapshot.Latency.t

val take_cancel_latency : t -> Snapshot.Latency.t
val take_reject_counts : t -> Snapshot.Reject_counts.t

val take_participant_activity
  :  t
  -> (Participant.t * Snapshot.Participant_activity.t) list

(** Register a stats subscriber. Same lifecycle as
    {!Dispatcher.subscribe_audit}: the pipe is dropped from the registry when
    the reader is closed. *)
val subscribe : t -> Snapshot.t Pipe.Reader.t

(** Push one snapshot to every subscriber (without pushback — snapshots are
    small and periodic, and a slow stats consumer must not stall the
    exchange). *)
val publish : t -> Snapshot.t -> unit
