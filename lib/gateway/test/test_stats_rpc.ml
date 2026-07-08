(** End-to-end test of the [exchange-stats] pipe RPC: boot a real server
    (with a fast stats period so the test doesn't wait wall-clock seconds),
    generate one submit and one cancel, and check the stream reports them.

    Raw snapshot values (times, latencies, memory) are nondeterministic, so
    the test prints a normalized view: counts, sign checks, and the
    pipe-occupancy fields that are deterministic. *)

open! Core
open! Async
open Jsip_types
open Jsip_gateway
open Jsip_stats
open Jsip_test_harness

(* Read snapshots until, cumulatively, both one submit and one cancel have
   been reported; return every snapshot read, newest first. Counters (latency
   counts, reject counts, activity) are drained per snapshot, so assertions
   below sum them across all snapshots read; gauges (resting orders) are read
   off the newest snapshot only. *)
let rec read_until_active reader ~snapshots_newest_first =
  match%bind Pipe.read reader with
  | `Eof -> failwith "stats pipe closed before reporting activity"
  | `Ok (snapshot : Snapshot.t) ->
    let snapshots_newest_first = snapshot :: snapshots_newest_first in
    let total f = List.sum (module Int) snapshots_newest_first ~f in
    let submit_total =
      total (fun (snapshot : Snapshot.t) ->
        snapshot.submit_latency.total_count)
    in
    let cancel_total =
      total (fun (snapshot : Snapshot.t) ->
        snapshot.cancel_latency.total_count)
    in
    (match submit_total >= 1 && cancel_total >= 1 with
     | true -> return (snapshots_newest_first, submit_total, cancel_total)
     | false -> read_until_active reader ~snapshots_newest_first)
;;

(* Sum drained per-snapshot counts of one kind across every snapshot read.
   [which] projects the (key * count) list out of each snapshot. *)
let summed_counts snapshots ~which ~compare_key =
  List.concat_map snapshots ~f:which
  |> List.sort_and_group ~compare:(fun (k1, _) (k2, _) -> compare_key k1 k2)
  |> List.map ~f:(fun group ->
    let key, _ = List.hd_exn group in
    key, List.sum (module Int) group ~f:snd)
;;

let%expect_test "exchange-stats reflects submits, cancels, and sessions" =
  Harness.reset_client_order_id_counter ();
  let%bind server =
    Exchange_server.start
      ~stats_period:(Time_ns.Span.of_ms 50.)
      ~symbols:[ Harness.aapl ]
      ~port:0
      ()
  in
  let port = Exchange_server.port server in
  Monitor.protect
    ~finally:(fun () -> Exchange_server.close server)
    (fun () ->
      (* Log in without subscribing to the session feed: the feed's events
         stay buffered, which is exactly what the [sessions] occupancy below
         counts. *)
      let%bind client = E2e_helpers.connect ~port in
      let conn = E2e_helpers.connection client in
      let%bind login_result =
        Rpc.Rpc.dispatch_exn Rpc_protocol.login_rpc conn "Alice"
      in
      let (_ : Participant.t) = Or_error.ok_exn login_result in
      let%bind snapshots, (_ : Rpc.Pipe_rpc.Metadata.t) =
        Rpc.Pipe_rpc.dispatch_exn Stats_protocol.stats_rpc conn ()
      in
      (* One resting buy (-> Order_accept to Alice's session) and one cancel
         of an unknown id (-> Cancel_reject to Alice's session). *)
      let%bind () =
        E2e_helpers.rpc_submit client (Harness.buy ~price_cents:10_000 ())
      in
      let%bind () =
        E2e_helpers.rpc_cancel
          client
          (Client_order_id.For_testing.of_int 999)
      in
      let%bind all_snapshots, submit_total, cancel_total =
        read_until_active snapshots ~snapshots_newest_first:[]
      in
      let final = List.hd_exn all_snapshots in
      let all_samples =
        Array.append
          final.submit_latency.samples
          final.cancel_latency.samples
      in
      let cancel_rejects =
        summed_counts
          all_snapshots
          ~which:(fun (snapshot : Snapshot.t) ->
            snapshot.reject_counts.cancel_rejects)
          ~compare_key:String.compare
      in
      let activity_counts ~f =
        summed_counts
          all_snapshots
          ~which:(fun (snapshot : Snapshot.t) ->
            List.map
              snapshot.participant_activity
              ~f:(fun (participant, activity) -> participant, f activity))
          ~compare_key:Participant.compare
      in
      let submits_by_participant =
        activity_counts
          ~f:(fun (activity : Snapshot.Participant_activity.t) ->
            activity.submits)
      in
      let cancels_by_participant =
        activity_counts
          ~f:(fun (activity : Snapshot.Participant_activity.t) ->
            activity.cancels)
      in
      print_s
        [%message
          ""
            (submit_total : int)
            (cancel_total : int)
            ~samples_are_nonnegative:
              (Array.for_all all_samples ~f:(fun span ->
                 Time_ns.Span.( >= ) span Time_ns.Span.zero)
               : bool)
            ~live_words_is_positive:(final.memory.live_words > 0 : bool)
            ~request_queue:(final.pipe_occupancy.request_queue : int)
            ~market_data:
              (final.pipe_occupancy.market_data : (Symbol.t * int list) list)
            ~audit:(final.pipe_occupancy.audit : int list)
            ~sessions:
              (final.pipe_occupancy.sessions : (Participant.t * int) list)
            (cancel_rejects : (string * int) list)
            (submits_by_participant : (Participant.t * int) list)
            (cancels_by_participant : (Participant.t * int) list)
            ~resting_orders:
              (final.resting_orders
               : (Participant.t * Snapshot.Resting_orders.t) list)];
      [%expect
        {|
        ((submit_total 1) (cancel_total 1) (samples_are_nonnegative true)
         (live_words_is_positive true) (request_queue 0) (market_data ()) (audit ())
         (sessions ((Alice 2))) (cancel_rejects (("order not found" 1)))
         (submits_by_participant ((Alice 1))) (cancels_by_participant ((Alice 1)))
         (resting_orders ((Alice ((order_count 1) (total_shares 100))))))
        |}];
      return ())
;;

let%expect_test "exchange-stats is reachable over websocket" =
  let%bind server =
    Exchange_server.start
      ~stats_period:(Time_ns.Span.of_ms 50.)
      ~http_port:0
      ~symbols:[ Harness.aapl ]
      ~port:0
      ()
  in
  Monitor.protect
    ~finally:(fun () -> Exchange_server.close server)
    (fun () ->
      let http_port = Option.value_exn (Exchange_server.http_port server) in
      let%bind conn =
        Rpc_websocket.Rpc.client
          (Uri.of_string [%string "ws://localhost:%{http_port#Int}/"])
        >>| ok_exn
      in
      let%bind snapshots, (_ : Rpc.Pipe_rpc.Metadata.t) =
        Rpc.Pipe_rpc.dispatch_exn Stats_protocol.stats_rpc conn ()
      in
      match%bind Pipe.read snapshots with
      | `Eof -> failwith "stats pipe closed before the first snapshot"
      | `Ok (snapshot : Snapshot.t) ->
        print_s
          [%message
            "snapshot received over websocket"
              ~live_words_is_positive:(snapshot.memory.live_words > 0 : bool)];
        [%expect
          {| ("snapshot received over websocket" (live_words_is_positive true)) |}];
        return ())
;;
