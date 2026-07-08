open! Core
open Jsip_types

module Memory = struct
  type t =
    { live_words : int
    ; heap_words : int
    ; top_heap_words : int
    ; minor_collections : int
    ; major_collections : int
    ; compactions : int
    }
  [@@deriving sexp, bin_io, compare, equal]

  let of_gc_stat (stat : Gc.Stat.t) =
    { live_words = stat.live_words
    ; heap_words = stat.heap_words
    ; top_heap_words = stat.top_heap_words
    ; minor_collections = stat.minor_collections
    ; major_collections = stat.major_collections
    ; compactions = stat.compactions
    }
  ;;

  module For_testing = struct
    let create
      ~live_words
      ~heap_words
      ~top_heap_words
      ~minor_collections
      ~major_collections
      ~compactions
      =
      { live_words
      ; heap_words
      ; top_heap_words
      ; minor_collections
      ; major_collections
      ; compactions
      }
    ;;
  end
end

module Latency = struct
  type t =
    { samples : Time_ns.Span.t array
    ; total_count : int
    }
  [@@deriving sexp, bin_io, compare, equal]

  let empty = { samples = [||]; total_count = 0 }
end

module Reject_counts = struct
  type t =
    { order_rejects : (string * int) list
    ; cancel_rejects : (string * int) list
    ; order_cancels : (Cancel_reason.t * int) list
    }
  [@@deriving sexp, bin_io, compare, equal]

  let empty = { order_rejects = []; cancel_rejects = []; order_cancels = [] }
end

module Participant_activity = struct
  type t =
    { submits : int
    ; cancels : int
    }
  [@@deriving sexp, bin_io, compare, equal]
end

module Resting_orders = struct
  type t =
    { order_count : int
    ; total_shares : Size.t
    }
  [@@deriving sexp, bin_io, compare, equal]
end

module Pipe_occupancy = struct
  type t =
    { request_queue : int
    ; market_data : (Symbol.t * int list) list
    ; audit : int list
    ; sessions : (Participant.t * int) list
    }
  [@@deriving sexp, bin_io, compare, equal]
end

type t =
  { time : Time_ns.Alternate_sexp.t
  ; memory : Memory.t
  ; submit_latency : Latency.t
  ; cancel_latency : Latency.t
  ; pipe_occupancy : Pipe_occupancy.t
  ; reject_counts : Reject_counts.t
  ; participant_activity : (Participant.t * Participant_activity.t) list
  ; resting_orders : (Participant.t * Resting_orders.t) list
  }
[@@deriving sexp, bin_io, compare, equal]
