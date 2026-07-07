open! Core
open Jsip_types
open Jsip_scenario_runner
module Spammer_bot = Jsip_bots.Spammer_bot
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

let name = "order-spam"

let description =
  "Adversarial: two spammers pound one book -- one resting buys, one \
   resting sells -- flooding the request queue without ever crossing."
;;

(* The single book both spammers target. *)
let symbol = Symbol.of_string "AAPL"
let buy_spammer = Participant.of_string "SpammerBuy"
let sell_spammer = Participant.of_string "SpammerSell"

(* How hard each spammer leans on the exchange. Both sides share this profile
   so the load is symmetric: same cadence, same depth, same size. A burst
   every tick with a deep [dist_from_fundamental_cents] means orders rest on
   the book rather than trading away, so the queue steadily fills. *)
let burst_interval = 1
let burst_size = 50
let dist_from_fundamental_cents = 100
let order_size = 10
let tick_interval = Time_ns.Span.of_sec 0.5

let oracle_config : Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( symbol
      , { Fundamental_oracle.Config.initial_price_cents = 15000
        ; volatility_cents_per_sec = 5.0
        ; mean_reversion_strength = 0.1
        ; tick_interval
        } )
    ]
;;

(* Wrap a spammer module + config into the existential [Bot_spec.t] the
   runner consumes. Each spammer gets its own client-order-id generator (so
   their ids never collide) and its own rng seed (for reproducibility). A
   spammer reads only the oracle's fundamental on each tick and never reacts
   to market data, so [is_marketdata_consumer] is [false]. *)
let spammer_spec ~participant ~(side : Side.t) ~rng_seed : Bot_spec.t =
  let config : Spammer_bot.Config.t =
    { participant
    ; symbols = [ symbol ]
    ; side
    ; burst_interval
    ; burst_size
    ; dist_from_fundamental_cents
    ; order_size
    ; client_order_id_generator = Client_order_id.Generator.create ()
    ; ticks_since_prev_burst = 0
    }
  in
  T
    { bot = (module Spammer_bot)
    ; config
    ; participant
    ; symbols = [ symbol ]
    ; rng_seed
    ; tick_interval
    ; is_marketdata_consumer = false
    }
;;

let buy_spammer_spec =
  spammer_spec ~participant:buy_spammer ~side:Buy ~rng_seed:1
;;

let sell_spammer_spec =
  spammer_spec ~participant:sell_spammer ~side:Sell ~rng_seed:2
;;

let configure () : Scenario_config.t =
  { name
  ; symbols = [ symbol ]
  ; oracle_config
  ; news = []
  ; bots = [ buy_spammer_spec; sell_spammer_spec ]
  }
;;
