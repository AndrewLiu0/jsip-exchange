open! Core
open! Async
open Jsip_types
open Jsip_stats

(* One latency accumulator (submit and cancel each get their own). Keeps the
   first [cap] samples of the interval and counts the rest — at the stats
   cadence (~1s) first-N is a good-enough sample of the interval, and it
   keeps [record] allocation-free once the queue has grown. *)
module Series = struct
  type t =
    { samples : Time_ns.Span.t Queue.t
    ; mutable total_count : int
    }

  let create () = { samples = Queue.create (); total_count = 0 }

  let record t span ~cap =
    t.total_count <- t.total_count + 1;
    if Queue.length t.samples < cap then Queue.enqueue t.samples span
  ;;

  let take t =
    let latency : Snapshot.Latency.t =
      { samples = Queue.to_array t.samples; total_count = t.total_count }
    in
    Queue.clear t.samples;
    t.total_count <- 0;
    latency
  ;;
end

(* Mutable per-participant interval counters; reset by
   [take_participant_activity] via [Hashtbl.clear] so an interval with no
   traffic reports an empty list rather than a table of zeros. *)
module Activity = struct
  type t =
    { mutable submits : int
    ; mutable cancels : int
    }

  let to_snapshot t : Snapshot.Participant_activity.t =
    { submits = t.submits; cancels = t.cancels }
  ;;
end

type t =
  { submit : Series.t
  ; cancel : Series.t
  ; max_samples_per_kind : int
  ; subscribers : Snapshot.t Pipe.Writer.t Bag.t
  ; order_rejects : int String.Table.t
  ; cancel_rejects : int String.Table.t
  ; order_cancels : (Cancel_reason.t, int) Hashtbl.t
  ; activity : Activity.t Participant.Table.t
  }

let default_max_samples_per_kind = 1_000

let create ?(max_samples_per_kind = default_max_samples_per_kind) () =
  { submit = Series.create ()
  ; cancel = Series.create ()
  ; max_samples_per_kind
  ; subscribers = Bag.create ()
  ; order_rejects = String.Table.create ()
  ; cancel_rejects = String.Table.create ()
  ; order_cancels = Hashtbl.create (module Cancel_reason)
  ; activity = Participant.Table.create ()
  }
;;

let find_activity t participant =
  Hashtbl.find_or_add t.activity participant ~default:(fun () ->
    { Activity.submits = 0; cancels = 0 })
;;

let record_submit_latency t ~participant span =
  Series.record t.submit span ~cap:t.max_samples_per_kind;
  let activity = find_activity t participant in
  activity.submits <- activity.submits + 1
;;

let record_cancel_latency t ~participant span =
  Series.record t.cancel span ~cap:t.max_samples_per_kind;
  let activity = find_activity t participant in
  activity.cancels <- activity.cancels + 1
;;

let record_events t events =
  let count table key = Hashtbl.incr table key in
  List.iter events ~f:(fun (event : Exchange_event.t) ->
    match event with
    | Order_reject { participant = _; request = _; reason } ->
      count t.order_rejects reason
    | Cancel_reject { participant = _; client_order_id = _; reason } ->
      count t.cancel_rejects reason
    | Order_cancel
        { order_id = _
        ; client_order_id = _
        ; participant = _
        ; symbol = _
        ; remaining_size = _
        ; reason
        } ->
      count t.order_cancels reason
    | Order_accept _ | Fill _ | Best_bid_offer_update _ | Trade_report _ ->
      ())
;;

let take_submit_latency t = Series.take t.submit
let take_cancel_latency t = Series.take t.cancel

(* Drain a counter table into a key-sorted alist, resetting it. Sorting keeps
   snapshots deterministic under expect tests (same reason the dispatcher
   sorts its queue-length lists). *)
let take_counts table ~compare_key =
  let counts =
    Hashtbl.to_alist table
    |> List.sort ~compare:(fun (k1, _) (k2, _) -> compare_key k1 k2)
  in
  Hashtbl.clear table;
  counts
;;

let take_reject_counts t : Snapshot.Reject_counts.t =
  { order_rejects = take_counts t.order_rejects ~compare_key:String.compare
  ; cancel_rejects = take_counts t.cancel_rejects ~compare_key:String.compare
  ; order_cancels =
      take_counts t.order_cancels ~compare_key:Cancel_reason.compare
  }
;;

let take_participant_activity t =
  let activity =
    Hashtbl.to_alist t.activity
    |> List.map ~f:(fun (participant, activity) ->
      participant, Activity.to_snapshot activity)
    |> List.sort ~compare:(fun (p1, _) (p2, _) -> Participant.compare p1 p2)
  in
  Hashtbl.clear t.activity;
  activity
;;

let subscribe t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.subscribers elt);
  reader
;;

let publish t snapshot =
  Bag.iter t.subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer snapshot)
;;
