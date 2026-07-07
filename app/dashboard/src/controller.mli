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

  (** Snapshots currently in the window (post-eviction). *)
  val window_length : t -> int
end
