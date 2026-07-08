(** Tests for the dashboard's pure {!Controller}: synthetic snapshots with
    fixed timestamps go in, the {!Controller.Display.t} sexp comes out. *)

open! Core
open Jsip_types
open Jsip_stats
open Jsip_dashboard

let time_at ~sec = Time_ns.add Time_ns.epoch (Time_ns.Span.of_int_sec sec)

(* A snapshot with only the fields a given test cares about; everything else
   is zero/empty. Latency samples are given in microseconds. *)
let snapshot
  ?(submit_us = [])
  ?submit_total
  ?(cancel_us = [])
  ?(live_words = 1_000)
  ?(rejects = Snapshot.Reject_counts.empty)
  ?(activity = [])
  ?(resting = [])
  ?(sessions = [])
  ~at_sec
  ()
  : Snapshot.t
  =
  let latency samples_us ~total : Snapshot.Latency.t =
    { samples = Array.of_list_map samples_us ~f:Time_ns.Span.of_int_us
    ; total_count = Option.value total ~default:(List.length samples_us)
    }
  in
  { time = time_at ~sec:at_sec
  ; memory =
      Snapshot.Memory.For_testing.create
        ~live_words
        ~heap_words:0
        ~top_heap_words:0
        ~minor_collections:0
        ~major_collections:0
        ~compactions:0
  ; submit_latency = latency submit_us ~total:submit_total
  ; cancel_latency = latency cancel_us ~total:None
  ; pipe_occupancy =
      { request_queue = 0; market_data = []; audit = []; sessions }
  ; reject_counts = rejects
  ; participant_activity = activity
  ; resting_orders = resting
  }
;;

let alice = Participant.of_string "Alice"
let bob = Participant.of_string "Bob"

let feed_all snapshots =
  List.fold
    snapshots
    ~init:(Controller.create ())
    ~f:Controller.feed_snapshot
;;

let show controller =
  print_s [%sexp (Controller.display controller : Controller.Display.t)]
;;

let%expect_test "an empty controller displays an empty dashboard" =
  show (Controller.create ());
  [%expect
    {|
    ((memory_series ()) (latest_memory ())
     (submit
      ((p50 ()) (p90 ()) (p99 ()) (window_sample_count 0) (window_total_count 0)))
     (cancel
      ((p50 ()) (p90 ()) (p99 ()) (window_sample_count 0) (window_total_count 0)))
     (occupancy ())
     (reject_totals ((order_rejects ()) (cancel_rejects ()) (order_cancels ())))
     (participants ()) (snapshots_received 0))
    |}]
;;

let%expect_test "percentiles over a known sample set" =
  (* Ten evenly spaced samples, 100us..1000us, fed out of order so the result
     also demonstrates that [percentile] sorts. *)
  let controller =
    feed_all
      [ snapshot
          ~at_sec:0
          ~submit_us:[ 500; 100; 1000; 300; 700; 200; 900; 400; 800; 600 ]
          ()
      ]
  in
  show controller;
  [%expect
    {|
    ((memory_series (("1970-01-01 00:00:00Z" 1000)))
     (latest_memory
      (((live_words 1000) (heap_words 0) (top_heap_words 0) (minor_collections 0)
        (major_collections 0) (compactions 0))))
     (submit
      ((p50 (500us)) (p90 (900us)) (p99 (1ms)) (window_sample_count 10)
       (window_total_count 10)))
     (cancel
      ((p50 ()) (p90 ()) (p99 ()) (window_sample_count 0) (window_total_count 0)))
     (occupancy (((request_queue 0) (market_data ()) (audit ()) (sessions ()))))
     (reject_totals ((order_rejects ()) (cancel_rejects ()) (order_cancels ())))
     (participants ()) (snapshots_received 1))
    |}]
;;

let%expect_test "percentiles pool samples across the window" =
  (* Per-snapshot medians are 1us and 9us. Averaging those medians would say
     5us; the pooled window [1;1;9;9] has a genuine median of 1us. This test
     exists so nobody "simplifies" the pooling away. *)
  let controller =
    feed_all
      [ snapshot ~at_sec:0 ~submit_us:[ 1; 1; 9 ] ()
      ; snapshot ~at_sec:1 ~submit_us:[ 9 ] ()
      ]
  in
  show controller;
  [%expect
    {|
    ((memory_series
      (("1970-01-01 00:00:00Z" 1000) ("1970-01-01 00:00:01Z" 1000)))
     (latest_memory
      (((live_words 1000) (heap_words 0) (top_heap_words 0) (minor_collections 0)
        (major_collections 0) (compactions 0))))
     (submit
      ((p50 (1us)) (p90 (9us)) (p99 (9us)) (window_sample_count 4)
       (window_total_count 4)))
     (cancel
      ((p50 ()) (p90 ()) (p99 ()) (window_sample_count 0) (window_total_count 0)))
     (occupancy (((request_queue 0) (market_data ()) (audit ()) (sessions ()))))
     (reject_totals ((order_rejects ()) (cancel_rejects ()) (order_cancels ())))
     (participants ()) (snapshots_received 2))
    |}]
;;

let%expect_test "server-side truncation surfaces in window_total_count" =
  (* The server observed 100 submits but shipped only 2 samples (its
     per-interval cap). The display must report both numbers. *)
  let controller =
    feed_all
      [ snapshot ~at_sec:0 ~submit_us:[ 10; 20 ] ~submit_total:100 () ]
  in
  show controller;
  [%expect
    {|
    ((memory_series (("1970-01-01 00:00:00Z" 1000)))
     (latest_memory
      (((live_words 1000) (heap_words 0) (top_heap_words 0) (minor_collections 0)
        (major_collections 0) (compactions 0))))
     (submit
      ((p50 (10us)) (p90 (20us)) (p99 (20us)) (window_sample_count 2)
       (window_total_count 100)))
     (cancel
      ((p50 ()) (p90 ()) (p99 ()) (window_sample_count 0) (window_total_count 0)))
     (occupancy (((request_queue 0) (market_data ()) (audit ()) (sessions ()))))
     (reject_totals ((order_rejects ()) (cancel_rejects ()) (order_cancels ())))
     (participants ()) (snapshots_received 1))
    |}]
;;

let%expect_test "sum_counts totals each reason across the window" =
  (* "rate limit exceeded" fires in two different snapshots and must come
     back as one entry with the window total; an empty interval contributes
     nothing.

     This expected value is written by hand, not promoted: the test is a
     specification, in the same spirit as the eviction test below. It fails
     (printing []) until [Controller.sum_counts] is implemented. The order
     shown here is alphabetical by reason — if you pick biggest-count-first
     instead, update this expectation with [--auto-promote] and read the
     diff. *)
  print_s
    [%sexp
      (Controller.For_testing.sum_counts
         [ [ "duplicate client order id", 1; "rate limit exceeded", 2 ]
         ; []
         ; [ "rate limit exceeded", 3; "unknown symbol", 1 ]
         ]
       : (string * int) list)];
  [%expect
    {|
    (("duplicate client order id" 1) ("rate limit exceeded" 5)
     ("unknown symbol" 1))
    |}]
;;

let%expect_test "participant rows join window counters with latest gauges" =
  (* Alice submits in both intervals (counter: summed to 3) and holds two
     resting orders in the newest snapshot (gauge: NOT summed with the older
     snapshot's value — the older reading is deliberately different to prove
     it is ignored). Bob appears only via a session with a backed-up pipe, so
     his row exists with zero activity — exactly how a slow-consumer-only
     participant shows up. *)
  let controller =
    feed_all
      [ snapshot
          ~at_sec:0
          ~activity:[ alice, { submits = 2; cancels = 1 } ]
          ~resting:
            [ alice, { order_count = 9; total_shares = Size.of_int 900 } ]
          ()
      ; snapshot
          ~at_sec:1
          ~activity:[ alice, { submits = 1; cancels = 0 } ]
          ~resting:
            [ alice, { order_count = 2; total_shares = Size.of_int 250 } ]
          ~sessions:[ alice, 0; bob, 47 ]
          ()
      ]
  in
  let { Controller.Display.participants; _ } =
    Controller.display controller
  in
  print_s
    [%sexp
      (participants : (Participant.t * Controller.Participant_row.t) list)];
  [%expect
    {|
    ((Alice
      ((resting_orders 2) (resting_shares 250) (submits 3) (cancels 1)
       (session_queue (0))))
     (Bob
      ((resting_orders 0) (resting_shares 0) (submits 0) (cancels 0)
       (session_queue (47)))))
    |}]
;;

let%expect_test "the window evicts snapshots older than window_span" =
  (* 70 snapshots one second apart. The newest is at t=69s, so with a 60s
     window everything at t=0..8s (strictly more than 60s older) must be
     evicted, keeping t=9..69s — 61 snapshots.

     This expected value is written by hand, not promoted: the test is a
     specification. It fails (reporting 70) until eviction is implemented in
     [Controller.feed_snapshot], then passes with no promotion needed. *)
  let controller =
    feed_all (List.init 70 ~f:(fun sec -> snapshot ~at_sec:sec ()))
  in
  print_s [%sexp (Controller.For_testing.window_length controller : int)];
  [%expect {| 61 |}]
;;
