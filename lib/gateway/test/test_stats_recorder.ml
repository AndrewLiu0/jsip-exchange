(** Tests for {!Jsip_gateway.Stats_recorder}: take-and-reset semantics, the
    per-interval sample cap, and subscriber delivery. *)

open! Core
open! Async
open Jsip_stats
open Jsip_gateway

let span_us us = Time_ns.Span.of_int_us us

let print_latency (latency : Snapshot.Latency.t) =
  print_s [%sexp (latency : Snapshot.Latency.t)]
;;

let%expect_test "take returns what was recorded, then resets" =
  let recorder = Stats_recorder.create () in
  Stats_recorder.record_submit_latency recorder (span_us 100);
  Stats_recorder.record_submit_latency recorder (span_us 250);
  Stats_recorder.record_cancel_latency recorder (span_us 40);
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
    Stats_recorder.record_submit_latency recorder (span_us us));
  print_latency (Stats_recorder.take_submit_latency recorder);
  [%expect {| ((samples (1us 2us 3us)) (total_count 5)) |}];
  return ()
;;

let empty_snapshot : Snapshot.t =
  { time = Time_ns.epoch
  ; memory =
      { live_words = 0
      ; heap_words = 0
      ; top_heap_words = 0
      ; minor_collections = 0
      ; major_collections = 0
      ; compactions = 0
      }
  ; submit_latency = Snapshot.Latency.empty
  ; cancel_latency = Snapshot.Latency.empty
  ; pipe_occupancy =
      { request_queue = 0; market_data = []; audit = []; sessions = [] }
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
