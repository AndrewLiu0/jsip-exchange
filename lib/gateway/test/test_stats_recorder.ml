(** Tests for {!Jsip_gateway.Stats_recorder}: take-and-reset semantics, the
    per-interval sample cap, reject/activity counters, and subscriber
    delivery. *)

open! Core
open! Async
open Jsip_types
open Jsip_stats
open Jsip_gateway
open Jsip_test_harness

let span_us us = Time_ns.Span.of_int_us us

let print_latency (latency : Snapshot.Latency.t) =
  print_s [%sexp (latency : Snapshot.Latency.t)]
;;

let%expect_test "take returns what was recorded, then resets" =
  let recorder = Stats_recorder.create () in
  Stats_recorder.record_submit_latency
    recorder
    ~participant:Harness.alice
    (span_us 100);
  Stats_recorder.record_submit_latency
    recorder
    ~participant:Harness.alice
    (span_us 250);
  Stats_recorder.record_cancel_latency
    recorder
    ~participant:Harness.alice
    (span_us 40);
  print_latency (Stats_recorder.take_submit_latency recorder);
  print_latency (Stats_recorder.take_cancel_latency recorder);
  (* A second take without new samples is empty: take resets. *)
  print_latency (Stats_recorder.take_submit_latency recorder);
  [%expect
    {|
    ((samples (100us 250us)) (total_count 2))
    ((samples (40us)) (total_count 1))
    ((samples ()) (total_count 0))
    |}];
  return ()
;;

let%expect_test "past the cap, samples stop but the count keeps going" =
  let recorder = Stats_recorder.create ~max_samples_per_kind:3 () in
  List.iter [ 1; 2; 3; 4; 5 ] ~f:(fun us ->
    Stats_recorder.record_submit_latency
      recorder
      ~participant:Harness.alice
      (span_us us));
  print_latency (Stats_recorder.take_submit_latency recorder);
  [%expect {| ((samples (1us 2us 3us)) (total_count 5)) |}];
  return ()
;;

let%expect_test "activity is counted per participant, then reset by take" =
  let recorder = Stats_recorder.create () in
  (* Two submits and a cancel for Alice, one submit for Bob. Latency spans
     are irrelevant here — zero keeps the focus on the counts. *)
  let zero = Time_ns.Span.zero in
  Stats_recorder.record_submit_latency
    recorder
    ~participant:Harness.alice
    zero;
  Stats_recorder.record_submit_latency
    recorder
    ~participant:Harness.alice
    zero;
  Stats_recorder.record_cancel_latency
    recorder
    ~participant:Harness.alice
    zero;
  Stats_recorder.record_submit_latency recorder ~participant:Harness.bob zero;
  let print_activity () =
    print_s
      [%sexp
        (Stats_recorder.take_participant_activity recorder
         : (Participant.t * Snapshot.Participant_activity.t) list)]
  in
  print_activity ();
  (* A second take reports an empty interval, not stale counts. *)
  print_activity ();
  [%expect
    {|
    ((Alice ((submits 2) (cancels 1))) (Bob ((submits 1) (cancels 0))))
    ()
    |}];
  return ()
;;

let%expect_test "events are counted by reject/cancel reason, then reset" =
  let recorder = Stats_recorder.create () in
  let request = Harness.buy ~price_cents:10_000 () in
  let reject reason : Exchange_event.t =
    Order_reject { participant = Harness.alice; request; reason }
  in
  let cancel reason : Exchange_event.t =
    Order_cancel
      { order_id = Order_id.For_testing.of_int 1
      ; client_order_id = request.client_order_id
      ; participant = Harness.alice
      ; symbol = request.symbol
      ; remaining_size = request.size
      ; reason
      }
  in
  Stats_recorder.record_events
    recorder
    [ reject "rate limit exceeded"
    ; reject "rate limit exceeded"
    ; reject "duplicate client_order_id"
    ; cancel Participant_requested
    ; cancel End_of_day
    ; cancel Participant_requested
    ; Cancel_reject
        { participant = Harness.alice
        ; client_order_id = request.client_order_id
        ; reason = "order not found"
        }
      (* Non-reject events must not count. *)
    ; Trade_report
        { symbol = request.symbol
        ; price = request.price
        ; size = request.size
        }
    ];
  let print_counts () =
    print_s
      [%sexp
        (Stats_recorder.take_reject_counts recorder
         : Snapshot.Reject_counts.t)]
  in
  print_counts ();
  print_counts ();
  [%expect
    {|
    ((order_rejects (("duplicate client_order_id" 1) ("rate limit exceeded" 2)))
     (cancel_rejects (("order not found" 1)))
     (order_cancels ((Participant_requested 2) (End_of_day 1))))
    ((order_rejects ()) (cancel_rejects ()) (order_cancels ()))
    |}];
  return ()
;;

let empty_snapshot : Snapshot.t =
  { time = Time_ns.epoch
  ; memory =
      Snapshot.Memory.For_testing.create
        ~live_words:0
        ~heap_words:0
        ~top_heap_words:0
        ~minor_collections:0
        ~major_collections:0
        ~compactions:0
  ; submit_latency = Snapshot.Latency.empty
  ; cancel_latency = Snapshot.Latency.empty
  ; pipe_occupancy =
      { request_queue = 0; market_data = []; audit = []; sessions = [] }
  ; reject_counts = Snapshot.Reject_counts.empty
  ; participant_activity = []
  ; resting_orders = []
  }
;;

let%expect_test "subscribers receive published snapshots" =
  let recorder = Stats_recorder.create () in
  let reader = Stats_recorder.subscribe recorder in
  Stats_recorder.publish recorder empty_snapshot;
  let%bind read_result = Pipe.read reader in
  (match read_result with
   | `Eof -> print_endline "eof"
   | `Ok snapshot ->
     print_s [%sexp (Snapshot.equal snapshot empty_snapshot : bool)]);
  [%expect {| true |}];
  return ()
;;
