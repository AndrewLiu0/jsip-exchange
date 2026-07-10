(** Scaffolding for bot tests. *)

open! Core
open! Async
open Jsip_types
open Jsip_fundamental
open Jsip_bot_runtime
open! Jsip_bots

let aapl = Symbol.of_string "AAPL"
let msft = Symbol.of_string "MSFT"
let alice = Participant.of_string "Alice"

(* Wire values carry ids; [make_recording_bot] gives the symbol at list
   position [i] the id [i], so [aapl] (always first) is id 0. *)
let aapl_id = Symbol_id.of_int_exn 0

(* A flat, deterministic oracle: every symbol sits at [initial_price_cents]
   and never moves (zero volatility, zero mean reversion). Because we drive
   [on_tick] by hand and never advance the oracle, the fundamental stays put,
   so every order's resting price is a fixed function of the config. *)
let oracle_config ~symbols ~initial_price_cents =
  Symbol.Map.of_alist_exn
    (List.map symbols ~f:(fun symbol ->
       ( symbol
       , { Fundamental_oracle.Config.initial_price_cents
         ; volatility_cents_per_sec = 0.0
         ; mean_reversion_strength = 0.0
         ; tick_interval = Time_ns.Span.of_sec 1.0
         } )))
;;

(* Build a runtime around a bot module with a mock submit/cancel that records
   what the bot does. [symbols] must cover every book the bot targets — the
   oracle raises on a symbol it has never heard of. *)
let make_recording_bot
  (type cfg)
  (bot_module : (module Bot_runtime.Bot with type Config.t = cfg))
  (config : cfg)
  ?(initial_price_cents = 15000)
  ?(symbols = [ aapl ])
  ()
  =
  let submitted = ref [] in
  let cancelled = ref [] in
  let submit request =
    submitted := request :: !submitted;
    return (Ok ())
  in
  let cancel order_id =
    cancelled := order_id :: !cancelled;
    return (Ok ())
  in
  let oracle =
    Fundamental_oracle.create
      (oracle_config ~symbols ~initial_price_cents)
      ~seed:42
  in
  let directory = Jsip_gateway.Symbol_directory.of_symbols symbols in
  let bot =
    Bot_runtime.create
      bot_module
      config
      ~participant:alice
      ~oracle
      ~symbol_id:(Jsip_gateway.Symbol_directory.id directory)
      ~symbol_name:(Jsip_gateway.Symbol_directory.name directory)
      ~rng:(Splittable_random.of_int 7)
      ~submit
      ~cancel
      ~tick_interval:(Time_ns.Span.of_sec 1.0)
  in
  bot, submitted, cancelled
;;

let print_submitted (submitted : Order.Request.t list ref) =
  let recent = List.rev !submitted in
  List.iter recent ~f:(fun req ->
    printf
      !"%{Side} %{Symbol_id} %d@%{Price#dollar} %{Time_in_force}\n"
      req.side
      req.symbol
      (Size.to_int req.size)
      req.price
      req.time_in_force)
;;

(* Smoke test: drive the do-nothing reference bot through one event so the
   runtest target exercises the helpers above. Replace or extend with
   bot-specific tests as concrete strategies are added to [Jsip_bots]. *)
module Inert_bot = struct
  module Config = struct
    type t = unit
  end

  let name = "inert"
  let on_start () _ctx = return ()
  let on_tick () _ctx = return ()
  let on_event () _ctx _event = return ()
end

let%expect_test "make_recording_bot wires up a runnable bot" =
  let bot, submitted, _cancelled =
    make_recording_bot (module Inert_bot) () ()
  in
  let%bind () =
    Bot_runtime.feed_event
      bot
      (Order_accept
         { order_id = Order_id.For_testing.of_int 1
         ; participant = alice
         ; request =
             { client_order_id = Client_order_id.of_int 1
             ; symbol = aapl_id
             ; side = Buy
             ; price = Price.of_int_cents 15000
             ; size = Size.of_int 10
             ; time_in_force = Day
             }
         })
  in
  print_submitted submitted;
  [%expect {| |}];
  return ()
;;

let bbo_event : Exchange_event.t =
  Best_bid_offer_update
    { symbol = aapl_id
    ; bbo =
        { bid =
            Some { price = Price.of_int_cents 14990; size = Size.of_int 100 }
        ; ask =
            Some { price = Price.of_int_cents 15010; size = Size.of_int 200 }
        }
    }
;;

(* The slow consumer drains [events_per_read] events immediately, then stalls
   for [read_delay] on the next one -- that stall is what backs up the
   exchange-side pipe. We assert the cadence without real sleeping:
   [read_delay] is large, so a stalled handler is simply a not-yet-determined
   deferred ([Deferred.peek = None]), and we read the [events_seen] counter
   (a ref we own) to confirm the increment and reset. *)
let%expect_test "slow consumer stalls every [events_per_read] events" =
  let events_seen = ref 0 in
  let config : Slow_consumer_bot.Config.t =
    { read_delay = Time_ns.Span.of_sec 60.
    ; events_per_read = 3
    ; events_seen
    }
  in
  let bot, _submitted, _cancelled =
    make_recording_bot (module Slow_consumer_bot) config ()
  in
  let feed_and_report label =
    let d = Bot_runtime.feed_event bot bbo_event in
    printf
      "%s: events_seen=%d stalled=%b\n"
      label
      !events_seen
      (Option.is_none (Deferred.peek d))
  in
  feed_and_report "event 1";
  feed_and_report "event 2";
  feed_and_report "event 3";
  feed_and_report "event 4";
  feed_and_report "event 5";
  feed_and_report "event 6";
  [%expect
    {|
    event 1: events_seen=1 stalled=false
    event 2: events_seen=2 stalled=false
    event 3: events_seen=0 stalled=true
    event 4: events_seen=1 stalled=false
    event 5: events_seen=2 stalled=false
    event 6: events_seen=0 stalled=true
    |}];
  return ()
;;

let%expect_test "book filler piles resting, non-marketable Day orders" =
  let config : Book_filler.Config.t =
    { symbols = [ aapl ]
    ; orders_per_tick = 4
    ; order_size = Size.of_int 10
    ; price_offset_cents = 50
    ; next_client_id = 1
    }
  in
  let bot, submitted, _cancelled =
    make_recording_bot
      (module Book_filler)
      config
      ~initial_price_cents:15000
      ()
  in
  let ctx = Bot_runtime.For_testing.context_of bot in
  (* Fundamental is 15000; buys must rest at 14950, sells at 15050 — none is
     marketable. Each tick emits [orders_per_tick] orders, alternating side. *)
  let%bind () = Book_filler.on_tick config ctx in
  let%bind () = Book_filler.on_tick config ctx in
  print_submitted submitted;
  [%expect
    {|
    BUY 0 10@$149.50 DAY
    SELL 0 10@$150.50 DAY
    BUY 0 10@$149.50 DAY
    SELL 0 10@$150.50 DAY
    BUY 0 10@$149.50 DAY
    SELL 0 10@$150.50 DAY
    BUY 0 10@$149.50 DAY
    SELL 0 10@$150.50 DAY
    |}];
  (* Every order must carry a fresh client_order_id, so none is rejected as a
     duplicate. Two ticks of 4 orders => 8 orders, 8 distinct ids. *)
  let ids =
    List.map !submitted ~f:(fun r ->
      Client_order_id.to_int r.client_order_id)
  in
  printf
    "orders=%d distinct_ids=%d\n"
    (List.length ids)
    (List.length (List.dedup_and_sort ids ~compare:Int.compare));
  [%expect {| orders=8 distinct_ids=8 |}];
  return ()
;;

(* --- Spammer bot -------------------------------------------------------- *)

(* Build a spammer config with sensible defaults so each test overrides only
   the axis it cares about. [ticks_since_start] and [next_burst_time] are
   reset by [on_start], so their initial values here are placeholders. *)
let spammer_config
  ?(symbols = [ aapl ])
  ?(side = Side.Buy)
  ?(dist_from_fundamental_cents = 50)
  ?(order_size = 10)
  ~burst_interval
  ~burst_size
  ()
  : Spammer_bot.Config.t
  =
  { participant = alice
  ; symbols
  ; side
  ; burst_interval
  ; burst_size
  ; dist_from_fundamental_cents
  ; order_size
  ; client_order_id_generator = Client_order_id.Generator.create ()
  ; ticks_since_prev_burst = 0
  }
;;

(* Run a spammer through [on_start] then [num_ticks] hand-driven ticks,
   invoking [on_each_tick] after every tick with the number of orders that
   tick produced. Returns the full record of submitted requests. *)
let run_spammer (config : Spammer_bot.Config.t) ~num_ticks ~on_each_tick =
  let bot, submitted, _cancelled =
    make_recording_bot (module Spammer_bot) config ~symbols:config.symbols ()
  in
  let ctx = Bot_runtime.For_testing.context_of bot in
  let%bind () = Spammer_bot.on_start config ctx in
  let%map () =
    Deferred.List.iter
      (List.init num_ticks ~f:(fun i -> i + 1))
      ~how:`Sequential
      ~f:(fun tick ->
        let before = List.length !submitted in
        let%map () = Spammer_bot.on_tick config ctx in
        on_each_tick ~tick ~sent_this_tick:(List.length !submitted - before))
  in
  submitted
;;

(* Print one line per tick showing how many orders fired — the shape we use
   to reason about burst timing and sizing. *)
let print_per_tick ~tick ~sent_this_tick =
  printf "tick %d: %d orders\n" tick sent_this_tick
;;

(* Like [print_submitted] but also shows the client order id, so we can see
   the generator hands out a fresh, non-colliding id per order. *)
let print_orders_with_ids (submitted : Order.Request.t list ref) =
  List.iter (List.rev !submitted) ~f:(fun (req : Order.Request.t) ->
    printf
      !"%{Symbol_id} %{Side} %d@%{Price#dollar} coid=%{Client_order_id}\n"
      req.symbol
      req.side
      (Size.to_int req.size)
      req.price
      req.client_order_id)
;;

let%expect_test "a burst targets every configured symbol with fresh ids" =
  let config =
    spammer_config ~symbols:[ aapl; msft ] ~burst_interval:1 ~burst_size:2 ()
  in
  (* One tick with burst_interval = 1 fires exactly one burst. *)
  let%bind submitted =
    run_spammer
      config
      ~num_ticks:1
      ~on_each_tick:(fun ~tick:_ ~sent_this_tick:_ -> ())
  in
  print_orders_with_ids submitted;
  (* 2 symbols x burst_size 2 = 4 orders; Buy rests 50c below the 15000
     fundamental at $149.50; every id is distinct. *)
  [%expect
    {|
    0 BUY 10@$149.50 coid=1
    0 BUY 10@$149.50 coid=2
    1 BUY 10@$149.50 coid=3
    1 BUY 10@$149.50 coid=4
    |}];
  return ()
;;

(* Unlike [print_submitted], this includes each request's client order id:
   the storm pairs submits with cancels by id, so the ids are the pattern
   under test. Prints cumulatively — the recording refs are never cleared —
   which makes id freshness across ticks visible in the expect output. *)
let print_storm_activity
  ~(submitted : Order.Request.t list ref)
  ~(cancelled : Client_order_id.t list ref)
  =
  List.iter (List.rev !submitted) ~f:(fun (req : Order.Request.t) ->
    printf
      !"submit %{Client_order_id}: %{Side} %{Symbol_id} %d@%{Price#dollar}\n"
      req.client_order_id
      req.side
      req.symbol
      (Size.to_int req.size)
      req.price);
  List.iter (List.rev !cancelled) ~f:(fun id ->
    printf !"cancel %{Client_order_id}\n" id)
;;

let storm_config ~side ~pairs_per_tick : Cancel_storm.Config.t =
  { symbol = aapl
  ; size = Size.of_int 1
  ; side
  ; offset_from_fundamental_cents = 50
  ; pairs_per_tick
  ; next_id = ref 0
  }
;;

let%expect_test "cancel storm: fresh submit/cancel pairs every tick" =
  let config = storm_config ~side:Side.Buy ~pairs_per_tick:2 in
  let bot, submitted, cancelled =
    make_recording_bot (module Cancel_storm) config ()
  in
  let ctx = Bot_runtime.For_testing.context_of bot in
  (* One tick: every submit is priced 50c below the (flat) fundamental of
     $150.00, and every submitted id is cancelled. *)
  let%bind () = Cancel_storm.on_tick config ctx in
  print_storm_activity ~submitted ~cancelled;
  [%expect
    {|
    submit 0: BUY 0 1@$149.50
    submit 1: BUY 0 1@$149.50
    cancel 0
    cancel 1
    |}];
  (* A second tick continues the id sequence — ids 2 and 3, never reusing 0
     or 1. The output below is cumulative across both ticks. *)
  let%bind () = Cancel_storm.on_tick config ctx in
  print_storm_activity ~submitted ~cancelled;
  [%expect
    {|
    submit 0: BUY 0 1@$149.50
    submit 1: BUY 0 1@$149.50
    submit 2: BUY 0 1@$149.50
    submit 3: BUY 0 1@$149.50
    cancel 0
    cancel 1
    cancel 2
    cancel 3
    |}];
  return ()
;;

(* The buy-side test above pins down pricing *below* the fundamental. The
   sell side is the mirror image: it must price *above* the fundamental so
   its orders rest instead of crossing the spread. *)
let%expect_test "cancel storm: sell side prices above the fundamental" =
  let config = storm_config ~side:Side.Sell ~pairs_per_tick:4 in
  let bot, submitted, cancelled =
    make_recording_bot (module Cancel_storm) config ()
  in
  let ctx = Bot_runtime.For_testing.context_of bot in
  let%bind () = Cancel_storm.on_tick config ctx in
  print_storm_activity ~submitted ~cancelled;
  [%expect
    {|
    submit 0: SELL 0 1@$150.50
    submit 1: SELL 0 1@$150.50
    submit 2: SELL 0 1@$150.50
    submit 3: SELL 0 1@$150.50
    cancel 0
    cancel 1
    cancel 2
    cancel 3
    |}];
  return ()
;;

(* ------------------------------------------------------------------------ *)
(* Tests for [Resource_canary_bot].

   The canary reads nothing from its [Context.t] — it drives latency probes
   through the [book_query] closure in its config. So we call [on_start] /
   [on_tick] directly with a throwaway context and a mock [book_query], then
   inspect the mutable [latency_data] table.

   - [request_interval] (probe every N ticks) is asserted via SAMPLE COUNTS,
     which are deterministic even though the recorded wall-clock latencies
     are not.
   - [report_interval] (print every M ticks) is asserted by PRE-SEEDING
     [latency_data] with known floats and freezing new probes (huge
     [request_interval]), so the printed report is fully deterministic. *)

module Rc = Resource_canary_bot

let msft = Symbol.of_string "MSFT"

(* The canary ignores the context, so any runnable bot's context will do. *)
let make_context () =
  let bot, _submitted, _cancelled =
    make_recording_bot (module Inert_bot) () ()
  in
  Bot_runtime.For_testing.context_of bot
;;

(* Build a config, start the bot, optionally seed [latency_data] (after
   [on_start], which clears it), then drive [ticks] ticks. Returns the config
   so callers can inspect the resulting state. *)
let run
  ~symbols
  ~request_interval
  ~report_interval
  ?(book_query = fun (_ : Symbol.t) -> return None)
  ?(seed = [])
  ~ticks
  ()
  : Rc.RCConfig.t Deferred.t
  =
  let ctx = make_context () in
  let config : Rc.RCConfig.t =
    { participant = alice
    ; request_interval
    ; report_interval
    ; symbols
    ; book_query
    ; latency_data = Symbol.Table.create ()
    ; ticks_since_start = 0
    }
  in
  let%bind () = Rc.on_start config ctx in
  List.iter seed ~f:(fun (symbol, data) ->
    Hashtbl.set config.latency_data ~key:symbol ~data);
  let%bind () =
    Deferred.List.iter
      (List.init ticks ~f:Fn.id)
      ~how:`Sequential
      ~f:(fun (_ : int) -> Rc.on_tick config ctx)
  in
  return config
;;

let print_sample_counts (config : Rc.RCConfig.t) : unit =
  List.iter config.symbols ~f:(fun symbol ->
    let data_length =
      Hashtbl.find_exn config.latency_data symbol
      |> Rc.For_testing.num_samples
    in
    print_endline [%string "%{symbol#Symbol}: %{data_length#Int} samples"])
;;

let%expect_test "requests are recorded every request_interval ticks, per \
                 symbol"
  =
  (* [report_interval] exceeds the tick count so no report fires; the only
     observable effect is the recorded sample counts. *)
  let show ~request_interval ~ticks =
    let%bind config =
      run
        ~symbols:[ aapl; msft ]
        ~request_interval
        ~report_interval:1_000_000
        ~ticks
        ()
    in
    printf "request_interval=%d, ticks=%d:\n" request_interval ticks;
    print_sample_counts config;
    return ()
  in
  let%bind () = show ~request_interval:1 ~ticks:6 in
  let%bind () = show ~request_interval:2 ~ticks:6 in
  let%bind () = show ~request_interval:3 ~ticks:7 in
  [%expect
    {|
    request_interval=1, ticks=6:
    AAPL: 6 samples
    MSFT: 6 samples
    request_interval=2, ticks=6:
    AAPL: 3 samples
    MSFT: 3 samples
    request_interval=3, ticks=7:
    AAPL: 2 samples
    MSFT: 2 samples
    |}];
  return ()
;;

let%expect_test "bursts fire every burst_interval ticks" =
  (* burst_interval = 3, one symbol, burst_size = 2: expect bursts on ticks 3
     and 6, nothing in between. *)
  let config =
    spammer_config ~symbols:[ aapl ] ~burst_interval:3 ~burst_size:2 ()
  in
  let%bind (_ : Order.Request.t list ref) =
    run_spammer config ~num_ticks:7 ~on_each_tick:print_per_tick
  in
  [%expect
    {|
    tick 1: 0 orders
    tick 2: 0 orders
    tick 3: 2 orders
    tick 4: 0 orders
    tick 5: 0 orders
    tick 6: 2 orders
    tick 7: 0 orders
    |}];
  return ()
;;

let%expect_test "burst_size controls how many orders each burst sends" =
  let config =
    spammer_config ~symbols:[ aapl ] ~burst_interval:1 ~burst_size:20 ()
  in
  let%bind _ =
    run_spammer config ~num_ticks:5 ~on_each_tick:print_per_tick
  in
  [%expect
    {|
  tick 1: 20 orders
  tick 2: 20 orders
  tick 3: 20 orders
  tick 4: 20 orders
  tick 5: 20 orders
  |}];
  return ()
;;

let%expect_test "report prints seeded latency stats once per report_interval"
  =
  (* Freeze new probes (huge [request_interval]) so the report reflects
     exactly the seeded data. The window keeps the most recent 3 samples (20,
     30, 40) while sum/count cover all four (10, 20, 30, 40): most_recent =
     40, avg = 100/4 = 25, last_3 = (20+30+40)/3 = 30. 7 ticks /
     report_interval 3 => two reports. *)
  let seeded =
    Rc.For_testing.create_data
      ~recent:[ 20.0; 30.0; 40.0 ]
      ~sum_latencies:100.0
      ~num_samples:4
  in
  let%bind (_ : Rc.RCConfig.t) =
    run
      ~symbols:[ aapl ]
      ~request_interval:1_000_000
      ~report_interval:3
      ~seed:[ aapl, seeded ]
      ~ticks:7
      ()
  in
  [%expect
    {|
    RESOURCE CANARY REPORT
    (AAPL) most_recent_latency_ms: 40.ms avg_latency_ms: 25.ms last_3_avg_latency_ms: 30.ms

    RESOURCE CANARY REPORT
    (AAPL) most_recent_latency_ms: 40.ms avg_latency_ms: 25.ms last_3_avg_latency_ms: 30.ms
    |}];
  return ()
;;
