open! Core
open! Async
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

type t =
  { submit : Series.t
  ; cancel : Series.t
  ; max_samples_per_kind : int
  ; subscribers : Snapshot.t Pipe.Writer.t Bag.t
  }

let default_max_samples_per_kind = 1_000

let create ?(max_samples_per_kind = default_max_samples_per_kind) () =
  { submit = Series.create ()
  ; cancel = Series.create ()
  ; max_samples_per_kind
  ; subscribers = Bag.create ()
  }
;;

let record_submit_latency t span =
  Series.record t.submit span ~cap:t.max_samples_per_kind
;;

let record_cancel_latency t span =
  Series.record t.cancel span ~cap:t.max_samples_per_kind
;;

let take_submit_latency t = Series.take t.submit
let take_cancel_latency t = Series.take t.cancel

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
