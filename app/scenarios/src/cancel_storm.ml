(** Pathological load scenario driving {!Jsip_bots.Cancel_storm}.

    One quiet symbol, no news, no other traffic: every RPC the exchange sees
    comes from the storm, so anything that degrades (latency, memory, feed
    backlog) is attributable to it. The interesting knobs are the storm's
    intensity and shape — see [storm_bot] and the lineup in [configure]. *)

open! Core
open Jsip_types
open Jsip_scenario_runner

let name = "cancel-storm"

let description =
  "Pathological submit/cancel storm on one quiet symbol, no other traffic."
;;

let symbol = Symbol.of_string "STRM"

(* Near-flat fundamental: the storm prices off the oracle, and a calm oracle
   keeps its passive orders from drifting into the spread and filling before
   they can be cancelled. *)
let oracle_config : Jsip_fundamental.Fundamental_oracle.Config.t =
  Symbol.Map.of_alist_exn
    [ ( symbol
      , { Jsip_fundamental.Fundamental_oracle.Config.initial_price_cents =
            10_000
        ; volatility_cents_per_sec = 2.0
        ; mean_reversion_strength = 0.1
        ; tick_interval = Time_ns.Span.of_ms 100.0
        } )
    ]
;;

(* One storm instance. [side] also names the participant, so a Buy and a Sell
   instance get distinct identities (and therefore distinct client-order-id
   spaces and session feeds). Each call allocates a fresh [next_id] ref — see
   [Cancel_storm.Config.next_id] for why instances must not share one. *)
let storm_bot ~(side : Side.t) ~rng_seed ~pairs_per_tick ~tick_interval
  : Bot_spec.t
  =
  let participant =
    Participant.of_string
      [%string "cancel-storm-%{String.lowercase (Side.to_string side)}"]
  in
  Bot_spec.T
    { bot = (module Jsip_bots.Cancel_storm)
    ; config =
        { Jsip_bots.Cancel_storm.Config.symbol
        ; size = Size.of_int 1
        ; side
        ; offset_from_fundamental_cents = 50
        ; pairs_per_tick
        ; next_id = ref 0
        }
    ; participant
    ; symbols = [ symbol ]
    ; rng_seed
    ; tick_interval
    ; is_marketdata_consumer = false
    }
;;

let configure () : Scenario_config.t =
  { name
  ; symbols = [ symbol ]
  ; oracle_config
  ; news = []
  ; bots =
      [ storm_bot
          ~side:Side.Buy
          ~rng_seed:3
          ~pairs_per_tick:40
          ~tick_interval:(Time_ns.Span.of_ms 50.)
      ; storm_bot
          ~side:Side.Sell
          ~rng_seed:4
          ~pairs_per_tick:40
          ~tick_interval:(Time_ns.Span.of_ms 50.)
      ]
  }
;;
