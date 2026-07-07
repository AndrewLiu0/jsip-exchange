(** One per-second metrics snapshot of the exchange, as streamed by
    {!Stats_protocol.stats_rpc}.

    Each snapshot is self-contained: the dashboard folds a window of them
    into its rolling view without needing any other RPC. Snapshots carry
    {e raw} latency samples rather than precomputed percentiles — the
    consumer computes percentiles over its whole window, because combining
    per-second percentiles (e.g. averaging sixty p99s) does not yield the
    window's p99.

    This library depends only on [core] and [async_rpc_kernel] so it links
    both into the native server and into the js_of_ocaml dashboard. Keep it
    that way: adding [async] here breaks the browser build. *)

open! Core
open Jsip_types

module Memory : sig
  (** The subset of [Gc.stat] the dashboard renders. [live_words] is the
      headline number: words currently reachable, i.e. the OCaml-side memory
      the exchange is really using (one word = 8 bytes on 64-bit). *)
  type t =
    { live_words : int
    ; heap_words : int (** Total words in the major heap, live or not. *)
    ; top_heap_words : int (** High-water mark of [heap_words]. *)
    ; minor_collections : int
    ; major_collections : int
    ; compactions : int
    }
  [@@deriving sexp, bin_io, compare, equal]

  val of_gc_stat : Gc.Stat.t -> t
end

module Latency : sig
  (** Raw latency samples observed during one snapshot interval. The producer
      caps [samples] (see {!Stats_recorder} in the gateway); [total_count] is
      the true number observed, so consumers can tell when sampling kicked in
      and show "n sampled of m". *)
  type t =
    { samples : Time_ns.Span.t array
    ; total_count : int
    }
  [@@deriving sexp, bin_io, compare, equal]

  val empty : t
end

module Pipe_occupancy : sig
  (** Queue lengths of every pipe inside the exchange that can back up. A
      slow consumer shows up here as one entry growing without bound. *)
  type t =
    { request_queue : int
    (** The matching engine's inbox: submits/cancels enqueued by RPC handlers
        but not yet matched. *)
    ; market_data : (Symbol.t * int list) list
    (** Per symbol, one length per subscriber pipe. *)
    ; audit : int list (** One length per audit-log subscriber. *)
    ; sessions : (Participant.t * int) list
    (** Per logged-in session's event feed. *)
    }
  [@@deriving sexp, bin_io, compare, equal]
end

type t =
  { time : Time_ns.Alternate_sexp.t
  (** Server clock at snapshot time. Consumers window on this, not on their
      own clock, so they are immune to clock skew. *)
  ; memory : Memory.t
  ; submit_latency : Latency.t
  ; cancel_latency : Latency.t
  ; pipe_occupancy : Pipe_occupancy.t
  }
[@@deriving sexp, bin_io, compare, equal]
