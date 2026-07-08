open! Core
open Jsip_types
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

module Reject_totals = struct
  type t =
    { order_rejects : (string * int) list
    ; cancel_rejects : (string * int) list
    ; order_cancels : (string * int) list
    }
  [@@deriving sexp_of, compare, equal]
end

module Participant_row = struct
  type t =
    { resting_orders : int
    ; resting_shares : Size.t
    ; submits : int
    ; cancels : int
    ; session_queue : int option
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
    ; reject_totals : Reject_totals.t
    ; participants : (Participant.t * Participant_row.t) list
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

(* Sum per-reason counts across the window. Each element of [per_snapshot] is
   one snapshot's drained (reason, count) list — the same reason can appear
   in many snapshots, and the result must have exactly one entry per distinct
   reason carrying the window total. This function also owns the pane's
   display order: alphabetical by reason and biggest-count-first are both
   defensible; pick one (ties need a deterministic order either way, or the
   expect tests flap). *)
let sum_counts (per_snapshot : (string * int) list list)
  : (string * int) list
  =
  let combined = List.concat per_snapshot in
  let reason_counts =
    Map.of_alist_fold (module String) combined ~init:0 ~f:( + )
  in
  Map.to_alist reason_counts
;;

let reject_totals snapshots : Reject_totals.t =
  let counts which =
    sum_counts
      (List.map snapshots ~f:(fun (snapshot : Snapshot.t) ->
         which snapshot.reject_counts))
  in
  { order_rejects =
      counts (fun (counts : Snapshot.Reject_counts.t) ->
        counts.order_rejects)
  ; cancel_rejects =
      counts (fun (counts : Snapshot.Reject_counts.t) ->
        counts.cancel_rejects)
  ; order_cancels =
      (* Stringified so the pane renders all three families identically. *)
      counts (fun (counts : Snapshot.Reject_counts.t) ->
        List.map counts.order_cancels ~f:(fun (reason, count) ->
          Cancel_reason.to_string reason, count))
  }
;;

let participant_rows snapshots ~(latest : Snapshot.t option)
  : (Participant.t * Participant_row.t) list
  =
  (* Counters: submits/cancels summed across every snapshot in the window. *)
  let activity_totals =
    List.fold
      snapshots
      ~init:Participant.Map.empty
      ~f:(fun totals (snapshot : Snapshot.t) ->
        List.fold
          snapshot.participant_activity
          ~init:totals
          ~f:
            (fun
              totals
              ( participant
              , { Snapshot.Participant_activity.submits; cancels } )
            ->
            Map.update totals participant ~f:(fun existing ->
              let previous_submits, previous_cancels =
                Option.value existing ~default:(0, 0)
              in
              previous_submits + submits, previous_cancels + cancels)))
  in
  (* Gauges: resting orders and session-pipe occupancy are levels, read off
     the newest snapshot only. *)
  let resting, session_queues =
    match latest with
    | None -> Participant.Map.empty, Participant.Map.empty
    | Some snapshot ->
      ( Participant.Map.of_alist_exn snapshot.resting_orders
      , Participant.Map.of_alist_exn snapshot.pipe_occupancy.sessions )
  in
  let all_participants =
    Participant.Set.union_list
      [ Map.key_set activity_totals
      ; Map.key_set resting
      ; Map.key_set session_queues
      ]
  in
  Set.to_list all_participants
  |> List.map ~f:(fun participant ->
    let submits, cancels =
      Option.value (Map.find activity_totals participant) ~default:(0, 0)
    in
    let resting_orders, resting_shares =
      match Map.find resting participant with
      | None -> 0, Size.zero
      | Some { Snapshot.Resting_orders.order_count; total_shares } ->
        order_count, total_shares
    in
    ( participant
    , { Participant_row.resting_orders
      ; resting_shares
      ; submits
      ; cancels
      ; session_queue = Map.find session_queues participant
      } ))
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
  ; reject_totals = reject_totals snapshots
  ; participants = participant_rows snapshots ~latest
  ; snapshots_received = t.snapshots_received
  }
;;

module For_testing = struct
  let percentile = percentile
  let sum_counts = sum_counts
  let window_length t = Fqueue.length t.window
end
