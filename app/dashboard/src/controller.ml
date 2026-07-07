open! Core
open Jsip_stats

module Latency_stats = struct
  type t =
    { p50 : Time_ns.Span.t option
    ; p90 : Time_ns.Span.t option
    ; p99 : Time_ns.Span.t option
    ; window_sample_count : int
    ; window_total_count : int
    }
  [@@deriving sexp_of, compare, equal]
end

module Display = struct
  type t =
    { memory_series : (Time_ns.Alternate_sexp.t * int) list
    ; latest_memory : Snapshot.Memory.t option
    ; submit : Latency_stats.t
    ; cancel : Latency_stats.t
    ; occupancy : Snapshot.Pipe_occupancy.t option
    ; snapshots_received : int
    }
  [@@deriving sexp_of, compare, equal]
end

type t =
  { window : Snapshot.t Fqueue.t (** Oldest snapshot at the front. *)
  ; snapshots_received : int
  }

let create () = { window = Fqueue.empty; snapshots_received = 0 }

(* Snapshots this much older than the newest one fall out of the window; the
   dashboard's percentiles and sparkline cover roughly this span. *)
let window_span = Time_ns.Span.of_sec 60.0

let feed_snapshot t (snapshot : Snapshot.t) =
  (* Snapshots arrive in time order, so the stale ones sit at the front of
     the queue; stop at the first one young enough to keep. A snapshot
     exactly [window_span] old is kept. *)
  let rec evict_queue queue =
    match Fqueue.peek queue with
    | None -> queue
    | Some (first : Snapshot.t) ->
      let age = Time_ns.diff snapshot.time first.time in
      if Time_ns.Span.( > ) age window_span
      then evict_queue (Fqueue.drop_exn queue)
      else queue
  in
  let window = evict_queue (Fqueue.enqueue t.window snapshot) in
  { window; snapshots_received = t.snapshots_received + 1 }
;;

(* Given the latency samples pooled across the whole window and a fraction
   [p] (0.5 for the median, 0.99 for p99), return the sample value at that
   percentile, or [None] if there are no samples.

   The standard "nearest-rank" method: sort the samples ascending, then
   return the element at index [ceil (p *. n) - 1], clamped into [0, n - 1].
   Linear interpolation between the two nearest samples is also defensible —
   pick one; the expect tests will pin your choice.

   The input array is freshly built by [display] for each call, so sorting it
   in place is fine ([Array.sort samples ~compare:Time_ns.Span.compare]).
   Watch the edges: p = 1.0 must not index past the end, and tiny [n] with
   small [p] must not index below 0. *)
let percentile (samples : Time_ns.Span.t array) ~p : Time_ns.Span.t option =
  match Array.length samples with
  | 0 -> None
  | n ->
    Array.sort samples ~compare:Time_ns.Span.compare;
    let index =
      Int.max 0 (Int.of_float (Float.round_up (p *. Float.of_int n)) - 1)
    in
    Some samples.(index)
;;

let latency_stats snapshots ~which : Latency_stats.t =
  let latencies : Snapshot.Latency.t list = List.map snapshots ~f:which in
  (* Pool the raw samples across every snapshot in the window, then take
     percentiles of the pool. Averaging each second's percentiles instead
     would weight quiet seconds equally with busy ones and cannot recover the
     true window percentile. *)
  let samples =
    Array.concat
      (List.map latencies ~f:(fun (latency : Snapshot.Latency.t) ->
         latency.samples))
  in
  { p50 = percentile samples ~p:0.5
  ; p90 = percentile samples ~p:0.9
  ; p99 = percentile samples ~p:0.99
  ; window_sample_count = Array.length samples
  ; window_total_count =
      List.sum
        (module Int)
        latencies
        ~f:(fun (latency : Snapshot.Latency.t) -> latency.total_count)
  }
;;

let display t : Display.t =
  let snapshots = Fqueue.to_list t.window in
  let latest = List.last snapshots in
  { memory_series =
      List.map snapshots ~f:(fun (snapshot : Snapshot.t) ->
        snapshot.time, snapshot.memory.live_words)
  ; latest_memory =
      Option.map latest ~f:(fun (snapshot : Snapshot.t) -> snapshot.memory)
  ; submit =
      latency_stats snapshots ~which:(fun (snapshot : Snapshot.t) ->
        snapshot.submit_latency)
  ; cancel =
      latency_stats snapshots ~which:(fun (snapshot : Snapshot.t) ->
        snapshot.cancel_latency)
  ; occupancy =
      Option.map latest ~f:(fun (snapshot : Snapshot.t) ->
        snapshot.pipe_occupancy)
  ; snapshots_received = t.snapshots_received
  }
;;

module For_testing = struct
  let percentile = percentile
  let window_length t = Fqueue.length t.window
end
