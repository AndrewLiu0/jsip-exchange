open! Core
open Jsip_types
open Jsip_order_book

let int_hum n = Int.to_string_hum ~delimiter:'_' n

(* One mutable counter per event kind (cancels split by reason): the fastest
   sanity check that a run did what its config prescribed. *)
module Event_counts = struct
  type t =
    { mutable accepts : int
    ; mutable fills : int
    ; mutable filled_size : int
    ; mutable cancels_requested : int
    ; mutable cancels_ioc_remainder : int
    ; mutable cancels_end_of_day : int
    ; mutable order_rejects : int
    ; mutable cancel_rejects : int
    ; mutable bbo_updates : int
    ; mutable trade_reports : int
    }

  let create () =
    { accepts = 0
    ; fills = 0
    ; filled_size = 0
    ; cancels_requested = 0
    ; cancels_ioc_remainder = 0
    ; cancels_end_of_day = 0
    ; order_rejects = 0
    ; cancel_rejects = 0
    ; bbo_updates = 0
    ; trade_reports = 0
    }
  ;;

  let add t events =
    List.iter events ~f:(fun (event : Exchange_event.t) ->
      match event with
      | Order_accept _ -> t.accepts <- t.accepts + 1
      | Fill fill ->
        t.fills <- t.fills + 1;
        t.filled_size <- t.filled_size + Size.to_int fill.size
      | Order_cancel { reason; _ } ->
        (match reason with
         | Participant_requested ->
           t.cancels_requested <- t.cancels_requested + 1
         | Ioc_remainder ->
           t.cancels_ioc_remainder <- t.cancels_ioc_remainder + 1
         | End_of_day -> t.cancels_end_of_day <- t.cancels_end_of_day + 1)
      | Order_reject _ -> t.order_rejects <- t.order_rejects + 1
      | Cancel_reject _ -> t.cancel_rejects <- t.cancel_rejects + 1
      | Best_bid_offer_update _ -> t.bbo_updates <- t.bbo_updates + 1
      | Trade_report _ -> t.trade_reports <- t.trade_reports + 1)
  ;;

  let print t =
    print_endline
      [%string
        {|events:
  accepts               : %{int_hum t.accepts}
  fills                 : %{int_hum t.fills} (total size %{int_hum t.filled_size})
  cancels (requested)   : %{int_hum t.cancels_requested}
  cancels (ioc leftover): %{int_hum t.cancels_ioc_remainder}
  cancels (end of day)  : %{int_hum t.cancels_end_of_day}
  order rejects         : %{int_hum t.order_rejects}
  cancel rejects        : %{int_hum t.cancel_rejects}
  bbo updates           : %{int_hum t.bbo_updates}
  trade reports         : %{int_hum t.trade_reports}|}]
  ;;
end

(* Nearest-rank: the value at 1-based rank [ceil (p/100 * n)], clamped into
   the array so p = 0 and p = 100 stay in range. Reports only latencies
   that were actually observed — no interpolation between elements. *)
let percentile (sorted : int array) ~p =
  let n = Array.length sorted in
  let rank = Float.to_int (Float.round_up (p /. 100. *. Float.of_int n)) in
  sorted.(Int.max 0 (Int.min (n - 1) (rank - 1)))
;;

(* Engine-side truth for the number of resting orders, to print against the
   generator's belief. Report-time only: walks every book. *)
let total_resting engine ~num_symbols =
  let total = ref 0 in
  for id = 0 to num_symbols - 1 do
    match Matching_engine.book engine (Symbol_id.of_int_exn id) with
    | None -> ()
    | Some book ->
      total
      := !total + Order_book.count book Buy + Order_book.count book Sell
  done;
  !total
;;

let print_progress
  ~engine
  ~generator
  ~(config : Workload.Config.t)
  ~actions_done
  =
  let resting = total_resting engine ~num_symbols:config.num_symbols in
  let bbo =
    match Matching_engine.book engine (Symbol_id.of_int_exn 0) with
    | None -> Bbo.empty
    | Some book -> Order_book.best_bid_offer book
  in
  let live_words = (Gc.stat ()).live_words in
  print_endline
    [%string
      "[%{int_hum actions_done}] resting=%{int_hum resting} \
       gen_live=%{int_hum (Workload.num_live generator)} \
       live_words=%{int_hum live_words} bbo0=%{Sexp.to_string [%sexp (bbo : \
       Bbo.t)]}"]
;;

let run ~(config : Workload.Config.t) ~seed ~num_actions ~report_every =
  let engine = Matching_engine.create ~num_symbols:config.num_symbols in
  let generator = Workload.create config ~seed in
  let latencies_ns = Array.create ~len:num_actions 0 in
  let counts = Event_counts.create () in
  let minor_words_before = Gc.minor_words () in
  let promoted_before = Gc.promoted_words () in
  let minor_collections_before = Gc.minor_collections () in
  let major_collections_before = Gc.major_collections () in
  let started = Time_ns.now () in
  for i = 0 to num_actions - 1 do
    let action = Workload.next_action generator in
    let before = Time_ns.now () in
    let events =
      match (action : Workload.Action.t) with
      | Submit { participant; request } ->
        Matching_engine.submit engine ~participant request
      | Cancel { participant; client_order_id } ->
        Matching_engine.cancel engine participant client_order_id
    in
    let after = Time_ns.now () in
    latencies_ns.(i) <- Time_ns.Span.to_int_ns (Time_ns.diff after before);
    Workload.observe generator events;
    Event_counts.add counts events;
    if report_every > 0 && (i + 1) % report_every = 0
    then print_progress ~engine ~generator ~config ~actions_done:(i + 1)
  done;
  let elapsed = Time_ns.diff (Time_ns.now ()) started in
  let elapsed_sec = Time_ns.Span.to_sec elapsed in
  Array.sort latencies_ns ~compare:Int.compare;
  let pct p = int_hum (percentile latencies_ns ~p) in
  print_endline
    [%string
      {|== replay report ==
actions      : %{int_hum num_actions} (seed %{seed#Int})
wall time    : %{Float.to_string_hum ~decimals:2 elapsed_sec}s
actions/sec  : %{int_hum (Float.to_int (Float.of_int num_actions /. elapsed_sec))}
latency (ns) : p50=%{pct 50.} p99=%{pct 99.} p99.9=%{pct 99.9} max=%{pct 100.}|}];
  Event_counts.print counts;
  print_endline
    [%string
      {|gc:
  minor words allocated : %{int_hum (Gc.minor_words () - minor_words_before)}
  promoted words        : %{int_hum (Gc.promoted_words () - promoted_before)}
  minor collections     : %{int_hum (Gc.minor_collections () - minor_collections_before)}
  major collections     : %{int_hum (Gc.major_collections () - major_collections_before)}|}];
  print_endline
    [%string
      "final book   : resting=%{int_hum (total_resting engine \
       ~num_symbols:config.num_symbols)} gen_live=%{int_hum \
       (Workload.num_live generator)}"]
;;

let config_of_preset preset =
  match String.lowercase preset with
  | "balanced" -> Workload.Config.balanced
  | other ->
    raise_s
      [%message
        "unknown preset (available: balanced)" ~preset:(other : string)]
;;

let command =
  Command.basic
    ~summary:
      "Pump generated workload actions straight into a matching engine and \
       report throughput, latency, and event counts"
    (let%map_open.Command num_actions =
       flag
         "-num-actions"
         (optional_with_default 1_000_000 int)
         ~doc:"N how many actions to replay (default 1_000_000)"
     and seed =
       flag
         "-seed"
         (optional_with_default 0 int)
         ~doc:"SEED root of all workload randomness (default 0)"
     and preset =
       flag
         "-preset"
         (optional_with_default "balanced" string)
         ~doc:"NAME workload preset (default balanced)"
     and report_every =
       flag
         "-report-every"
         (optional_with_default 100_000 int)
         ~doc:
           "N print a progress line every N actions, 0 to disable (default \
            100_000)"
     in
     fun () ->
       run ~config:(config_of_preset preset) ~seed ~num_actions ~report_every)
;;
