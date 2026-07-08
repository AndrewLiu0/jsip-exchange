(** Pure state for the web dashboard: folds the per-second
    {!Jsip_stats.Snapshot.t} stream into a rolling ~60s window and projects
    the view-model each pane renders.

    Mirrors the split in [app/monitor]: this module has zero Bonsai or Async
    dependencies, so it is testable as plain data — feed snapshots in with
    {!feed_snapshot}, look at {!display}. The Bonsai layer ({!Web_app}) owns
    the action variant and rendering.

    The window is bounded by each snapshot's own [time] field (the server
    clock), not by the browser's clock, so windowing is immune to clock skew
    and fully deterministic under test. *)

open! Core
open Jsip_types
open Jsip_stats

module Latency_stats : sig
  (** Percentiles over {e all} samples pooled across the window — never an
      average of per-second percentiles, which would be statistically
      meaningless. [None] when the window holds no samples.

      [window_total_count] includes samples the server dropped past its
      per-second cap, so the UI can show "n sampled of m". *)
  type t =
    { p50 : Time_ns.Span.t option
    ; p90 : Time_ns.Span.t option
    ; p99 : Time_ns.Span.t option
    ; window_sample_count : int
    ; window_total_count : int
    }
  [@@deriving sexp_of, compare, equal]
end

module Reject_totals : sig
  (** Reject/cancel reason counts summed across the whole window — "how many
      times did each reason fire in the last ~60s". [order_cancels] keys are
      stringified {!Cancel_reason.t}s so all three families render alike. *)
  type t =
    { order_rejects : (string * int) list
    ; cancel_rejects : (string * int) list
    ; order_cancels : (string * int) list
    }
  [@@deriving sexp_of, compare, equal]
end

module Participant_row : sig
  (** One participant's resource usage. Gauge fields ([resting_orders],
      [resting_shares], [session_queue]) are read off the {e newest} snapshot
      — summing a level across time is meaningless; counter fields
      ([submits], [cancels]) are summed across the window, i.e. "in the last
      ~60s". [session_queue] is [None] when the participant has no logged-in
      session (resting orders can outlive a disconnect). *)
  type t =
    { resting_orders : int
    ; resting_shares : Size.t
    ; submits : int
    ; cancels : int
    ; session_queue : int option
    }
  [@@deriving sexp_of, compare, equal]
end

module Display : sig
  (** Pure view-model, one field per pane. Decoupled from any Bonsai type so
      rendering functions are plain [Display.t -> Vdom.Node.t]. *)
  type t =
    { memory_series : (Time_ns.Alternate_sexp.t * int) list
    (** [(time, live_words)] per snapshot, oldest first — the memory
        sparkline. *)
    ; latest_memory : Snapshot.Memory.t option
    ; submit : Latency_stats.t
    ; cancel : Latency_stats.t
    ; occupancy : Snapshot.Pipe_occupancy.t option (** Latest snapshot's. *)
    ; reject_totals : Reject_totals.t
    ; participants : (Participant.t * Participant_row.t) list
    (** Sorted by participant: everyone with any activity, resting order, or
        session anywhere in the window. *)
    ; snapshots_received : int
    (** Lifetime count, not windowed — a liveness indicator. *)
    }
  [@@deriving sexp_of, compare, equal]
end

type t

val create : unit -> t

(** How far back the rolling window reaches: snapshots more than this much
    older than the newest one are dropped by {!feed_snapshot}. Exposed so the
    UI can label the panes (e.g. "last 60s"). *)
val window_span : Time_ns.Span.t

(** Append a snapshot and drop window entries older than {!window_span}
    (judged by the snapshots' own timestamps). *)
val feed_snapshot : t -> Snapshot.t -> t

val display : t -> Display.t

module For_testing : sig
  (** The percentile function used by {!display}; exposed so tests can pin
      its exact semantics on hand-picked sample sets. *)
  val percentile : Time_ns.Span.t array -> p:float -> Time_ns.Span.t option

  (** The per-reason summing used by {!display} for {!Reject_totals}; exposed
      so tests can pin its totals and its display order. *)
  val sum_counts : (string * int) list list -> (string * int) list

  (** Snapshots currently in the window (post-eviction). *)
  val window_length : t -> int
end
