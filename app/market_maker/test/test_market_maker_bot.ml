(** Tests for {!Market_maker_bot}, driving the bot's callbacks directly with
    recording [submit]/[cancel] closures — no exchange server involved. The
    "exchange" here is the test itself: we echo [Order_accept]s and inject
    [Fill]s by hand, exactly the mock-connection pattern from
    [app/bots/test/test_bots.ml]. *)

open! Core
open! Async
open Jsip_types
open Jsip_fundamental
open Jsip_bot_runtime
open Jsip_test_harness
open Jsip_market_maker

(* Everything the bot asks the exchange to do, in dispatch order. *)
module Action = struct
  type t =
    | Submit of Order.Request.t
    | Cancel of Client_order_id.t
end

let oracle_config ~initial_price_cents =
  Symbol.Map.of_alist_exn
    [ ( Harness.aapl
      , { Fundamental_oracle.Config.initial_price_cents
        ; volatility_cents_per_sec = 0.0
        ; mean_reversion_strength = 0.0
        ; tick_interval = Time_ns.Span.of_sec 1.0
        } )
    ]
;;

(* The oracle is created but never started, so the fundamental stays pinned
   at [initial_price_cents] and skew is the only thing moving the quotes. *)
let make_bot ?(inventory_skew_cents_per_share = 1) () =
  let config =
    Market_maker_bot.Config.create
      ~symbol:Harness.aapl
      ~half_spread_cents:10
      ~size_per_level:100
      ~num_levels:2
      ~inventory_skew_cents_per_share
  in
  let actions = ref [] in
  let submit request =
    actions := Action.Submit request :: !actions;
    return (Ok ())
  in
  let cancel client_order_id =
    actions := Action.Cancel client_order_id :: !actions;
    return (Ok ())
  in
  let oracle =
    Fundamental_oracle.create
      (oracle_config ~initial_price_cents:15000)
      ~seed:42
  in
  let bot =
    Bot_runtime.create
      (module Market_maker_bot)
      config
      ~participant:Harness.market_maker
      ~oracle
      ~rng:(Splittable_random.of_int 7)
      ~submit
      ~cancel
      ~tick_interval:(Time_ns.Span.of_sec 1.0)
  in
  config, bot, actions
;;

(* Take (and clear) everything the bot has dispatched since the last drain. *)
let drain actions =
  let recent = List.rev !actions in
  actions := [];
  recent
;;

let print_actions acts =
  List.iter acts ~f:(function
    | Action.Submit r ->
      printf
        !"SUBMIT %{Side} %d@%{Price#dollar} id=%{Client_order_id}\n"
        r.side
        (Size.to_int r.size)
        r.price
        r.client_order_id
    | Action.Cancel id -> printf !"CANCEL id=%{Client_order_id}\n" id)
;;

let submits acts =
  List.filter_map acts ~f:(function
    | Action.Submit r -> Some r
    | Cancel (_ : Client_order_id.t) -> None)
;;

let print_state config =
  let outstanding =
    Market_maker_bot.For_testing.outstanding config
    |> List.map ~f:Client_order_id.to_string
    |> String.concat ~sep:","
  in
  printf
    "inventory=%d outstanding=[%s]\n"
    (Market_maker_bot.For_testing.inventory config)
    outstanding
;;

(* Play the exchange: acknowledge a submitted request back to the bot. *)
let accept bot (request : Order.Request.t) =
  Bot_runtime.feed_event
    bot
    (Order_accept
       { order_id = Order_id.For_testing.of_int 0
       ; participant = Harness.market_maker
       ; request
       })
;;

let fill_against ~(resting : Order.Request.t) ~aggressor_side ~size : Fill.t =
  { fill_id = 1
  ; symbol = resting.symbol
  ; price = resting.price
  ; size
  ; aggressor_order_id = Order_id.For_testing.of_int 99
  ; aggressor_client_order_id = Client_order_id.For_testing.of_int 99
  ; aggressor_participant = Harness.alice
  ; aggressor_side
  ; resting_order_id = Order_id.For_testing.of_int 1
  ; resting_client_order_id = resting.client_order_id
  ; resting_participant = Harness.market_maker
  }
;;

let start_and_accept_ladder config bot actions =
  let ctx = Bot_runtime.For_testing.context_of bot in
  let%bind () = Market_maker_bot.on_start config ctx in
  let ladder = submits (drain actions) in
  let%map () = Deferred.List.iter ~how:`Sequential ladder ~f:(accept bot) in
  ladder
;;

let tightest ~side ladder =
  List.find_exn ladder ~f:(fun (r : Order.Request.t) ->
    match r.side, side with
    | Side.Buy, Side.Buy | Sell, Sell -> true
    | Buy, Sell | Sell, Buy -> false)
;;

let%expect_test "on_start posts a symmetric ladder around the fundamental" =
  let config, bot, actions = make_bot () in
  let ctx = Bot_runtime.For_testing.context_of bot in
  let%bind () = Market_maker_bot.on_start config ctx in
  print_actions (drain actions);
  (* Nothing is outstanding yet: the bot only trusts an order to be resting
     once the exchange echoes an [Order_accept]. *)
  print_state config;
  [%expect
    {|
    SUBMIT BUY 100@$149.90 id=1
    SUBMIT SELL 100@$150.10 id=2
    SUBMIT BUY 100@$149.89 id=3
    SUBMIT SELL 100@$150.11 id=4
    inventory=0 outstanding=[]
    |}];
  return ()
;;

let%expect_test "accepts and cancels maintain the outstanding-order set" =
  let config, bot, actions = make_bot () in
  let%bind ladder = start_and_accept_ladder config bot actions in
  print_state config;
  let first = List.hd_exn ladder in
  let%bind () =
    Bot_runtime.feed_event
      bot
      (Order_cancel
         { order_id = Order_id.For_testing.of_int 0
         ; client_order_id = first.client_order_id
         ; participant = Harness.market_maker
         ; symbol = first.symbol
         ; remaining_size = first.size
         ; reason = Participant_requested
         })
  in
  print_state config;
  [%expect
    {|
    inventory=0 outstanding=[1,2,3,4]
    inventory=0 outstanding=[2,3,4]
    |}];
  return ()
;;

(* Selling 100 at the ask should leave inventory=-100, cancel ids 1-4, and
   re-quote 100 cents higher (skew of 1 cent/share); the second test's
   buy-back should return the quotes to the original prices. *)

let%expect_test "a fill cancels everything and re-quotes with skew" =
  let config, bot, actions = make_bot () in
  let%bind ladder = start_and_accept_ladder config bot actions in
  (* Alice lifts our tightest ask: we sold 100 shares. *)
  let%bind () =
    Bot_runtime.feed_event
      bot
      (Fill
         (fill_against
            ~resting:(tightest ~side:Sell ladder)
            ~aggressor_side:Buy
            ~size:(Size.of_int 100)))
  in
  print_state config;
  print_actions (drain actions);
  [%expect
    {|
    inventory=-100 outstanding=[]
    CANCEL id=1
    CANCEL id=3
    CANCEL id=4
    SUBMIT BUY 100@$150.90 id=5
    SUBMIT SELL 100@$151.10 id=6
    SUBMIT BUY 100@$150.89 id=7
    SUBMIT SELL 100@$151.11 id=8
    |}];
  return ()
;;

let%expect_test "alternating fills oscillate symmetrically around fair" =
  let config, bot, actions = make_bot () in
  let%bind ladder = start_and_accept_ladder config bot actions in
  let fill_and_requote ~resting ~aggressor_side =
    let%bind () =
      Bot_runtime.feed_event
        bot
        (Fill (fill_against ~resting ~aggressor_side ~size:(Size.of_int 100)))
    in
    print_state config;
    let requoted = drain actions in
    print_actions requoted;
    let%map () =
      Deferred.List.iter ~how:`Sequential (submits requoted) ~f:(accept bot)
    in
    submits requoted
  in
  (* Alice lifts our ask (we sell), then hits the re-quoted bid (we buy):
     inventory should go -100 and back to 0, quotes up and back down. *)
  let%bind requoted =
    fill_and_requote
      ~resting:(tightest ~side:Sell ladder)
      ~aggressor_side:Buy
  in
  let%bind (_ : Order.Request.t list) =
    fill_and_requote
      ~resting:(tightest ~side:Buy requoted)
      ~aggressor_side:Sell
  in
  [%expect
    {|
    inventory=-100 outstanding=[]
    CANCEL id=1
    CANCEL id=3
    CANCEL id=4
    SUBMIT BUY 100@$150.90 id=5
    SUBMIT SELL 100@$151.10 id=6
    SUBMIT BUY 100@$150.89 id=7
    SUBMIT SELL 100@$151.11 id=8
    inventory=0 outstanding=[]
    CANCEL id=6
    CANCEL id=7
    CANCEL id=8
    SUBMIT BUY 100@$149.90 id=9
    SUBMIT SELL 100@$150.10 id=10
    SUBMIT BUY 100@$149.89 id=11
    SUBMIT SELL 100@$150.11 id=12
    |}];
  return ()
;;
