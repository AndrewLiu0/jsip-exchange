(** Tests for {!Jsip_stats.Snapshot}. The expect output below doubles as
    documentation of what one stats snapshot looks like on the wire (in sexp
    form; the RPC itself ships bin_io). *)

open! Core
open Jsip_test_harness
open Jsip_stats

let sample_snapshot : Snapshot.t =
  let time =
    (* A fixed instant so the expect output is deterministic. *)
    Time_ns.add Time_ns.epoch (Time_ns.Span.of_int_sec 1_000)
  in
  { time
  ; memory =
      { live_words = 50_000
      ; heap_words = 120_000
      ; top_heap_words = 150_000
      ; minor_collections = 42
      ; major_collections = 3
      ; compactions = 0
      }
  ; submit_latency =
      { samples = Array.map [| 120; 450; 90 |] ~f:Time_ns.Span.of_int_us
      ; total_count = 3
      }
  ; cancel_latency = Snapshot.Latency.empty
  ; pipe_occupancy =
      { request_queue = 2
      ; market_data = [ Harness.aapl, [ 0; 17 ] ]
      ; audit = [ 5 ]
      ; sessions = [ Harness.alice, 1 ]
      }
  }
;;

let%expect_test "snapshot sexp round-trips" =
  let sexp = [%sexp (sample_snapshot : Snapshot.t)] in
  print_s sexp;
  let round_tripped = [%of_sexp: Snapshot.t] sexp in
  print_s [%sexp (Snapshot.equal round_tripped sample_snapshot : bool)];
  [%expect
    {|
    ((time "1970-01-01 00:16:40Z")
     (memory
      ((live_words 50000) (heap_words 120000) (top_heap_words 150000)
       (minor_collections 42) (major_collections 3) (compactions 0)))
     (submit_latency ((samples (120us 450us 90us)) (total_count 3)))
     (cancel_latency ((samples ()) (total_count 0)))
     (pipe_occupancy
      ((request_queue 2) (market_data ((AAPL (0 17)))) (audit (5))
       (sessions ((Alice 1))))))
    true
    |}]
;;
