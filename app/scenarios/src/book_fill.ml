open! Core
open Jsip_types
open Jsip_scenario_runner
module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle
module Book_filler = Jsip_bots.Book_filler

let name = "book-fill"

let description =
  "Several book fillers pile resting Day orders without trading, driving \
   order-book memory and match latency up."
;;

(* Two symbols so the filler's round-robin is exercised. *)
let symbols = [ Symbol.of_string "AAPL"; Symbol.of_string "MSFT" ]

(* Intensity knobs. Three fillers each firing 50 orders every 200ms is ~750
   resting orders/sec, enough for memory growth to show within 30s. *)
let num_fillers = 3
let orders_per_tick = 50
let order_size = Size.of_int 10
let price_offset_cents = 50
let tick_interval = Time_ns.Span.of_ms 200.

(* Calm oracle: the pathology doesn't depend on price movement. *)
let oracle_config : Fundamental_oracle.Config.t =
  List.map symbols ~f:(fun symbol ->
    ( symbol
    , { Fundamental_oracle.Config.initial_price_cents = 15000
      ; volatility_cents_per_sec = 0.
      ; mean_reversion_strength = 0.
      ; tick_interval = Time_ns.Span.of_sec 1.
      } ))
  |> Symbol.Map.of_alist_exn
;;

let filler_spec index : Bot_spec.t =
  let config : Book_filler.Config.t =
    { symbols
    ; orders_per_tick
    ; order_size
    ; price_offset_cents
    ; next_client_id = 1
    }
  in
  T
    { bot = (module Book_filler)
    ; config
    ; participant =
        Participant.of_string [%string "book-filler-%{index#Int}"]
    ; symbols
    ; rng_seed = index
    ; tick_interval
    ; is_marketdata_consumer = false
    }
;;

let configure () : Scenario_config.t =
  { name
  ; symbols
  ; oracle_config
  ; news = []
  ; bots = List.init num_fillers ~f:filler_spec
  }
;;
