open! Core
open Jsip_types
open Jsip_scenario_runner
module Bot_runtime = Jsip_bot_runtime.Bot_runtime
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

let name = "slow-consumers"

let description =
  "One symbol with a market maker, noise trader, plus several slow \
   consumers. The exchange-side subscriber buffers should back up because \
   market data is written without pushback."
;;

let symbol = Symbol.of_string "AAPL"
let fair_value_cents = 15_000

let oracle_config : Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( symbol
      , { Fundamental_oracle.Config.initial_price_cents = fair_value_cents
        ; volatility_cents_per_sec = 10.0
        ; mean_reversion_strength = 0.05
        ; tick_interval = Time_ns.Span.of_ms 250.0
        } )
    ]
;;

(* One market maker to initially seed the book so the noise trader's orders
   have something to cross against to produce live BBO updates. Market maker
   never re-quotes on tick. *)
let market_maker_spec () =
  let config : Jsip_bots.Market_maker_bot.Config.t =
    { symbol
    ; fair_value_cents
    ; half_spread_cents = 5
    ; size_per_level = 100
    ; num_levels = 30
    ; fill_client_oid = ref 900
    ; inventory = Symbol.Map.empty
    ; currently_resting_orders = Client_order_id.Map.empty
    }
  in
  Bot_spec.T
    { bot =
        (module Jsip_bots.Market_maker_bot : Bot_runtime.Bot
          with type Config.t = Jsip_bots.Market_maker_bot.Config.t)
    ; config
    ; participant = Participant.of_string "market-maker"
    ; symbols = [ symbol ]
    ; rng_seed = 1
    ; tick_interval = Time_ns.Span.of_sec 1.0
    ; is_marketdata_consumer = false
    }
;;

(* Fires 10 orders/sec. *)
let noise_trader_spec () =
  let config : Jsip_bots.Tick_trader.Config.t =
    { symbol
    ; order_size = 10
    ; price_jitter_cents = 20
    ; next_client_order_id = ref 1
    }
  in
  Bot_spec.T
    { bot =
        (module Jsip_bots.Tick_trader : Bot_runtime.Bot
          with type Config.t = Jsip_bots.Tick_trader.Config.t)
    ; config
    ; participant = Participant.of_string "noise-trader"
    ; symbols = [ symbol ]
    ; rng_seed = 2
    ; tick_interval = Time_ns.Span.of_ms 100.0
    ; is_marketdata_consumer = false
    }
;;

let num_slow_consumers = 5
let read_delay = Time_ns.Span.of_sec 30.0
let events_per_read = 1

(* Reads 1 event per 30 seconds *)
let slow_consumer_spec index =
  let config : Jsip_bots.Slow_consumer_bot.Config.t =
    { read_delay; events_per_read; events_seen = ref 0 }
  in
  Bot_spec.T
    { bot =
        (module Jsip_bots.Slow_consumer_bot : Bot_runtime.Bot
          with type Config.t = Jsip_bots.Slow_consumer_bot.Config.t)
    ; config
    ; participant =
        Participant.of_string [%string "slow-consumer-%{index#Int}"]
    ; symbols = [ symbol ]
    ; rng_seed = 100 + index
    ; tick_interval = Time_ns.Span.of_sec 1.0
    ; is_marketdata_consumer = true
    }
;;

let configure () : Scenario_config.t =
  { name
  ; symbols = [ symbol ]
  ; oracle_config
  ; news = []
  ; bots =
      market_maker_spec ()
      :: noise_trader_spec ()
      :: List.init num_slow_consumers ~f:slow_consumer_spec
  }
;;
