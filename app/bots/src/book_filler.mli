open! Core
open! Async
open Jsip_types

(** Book filler: a pathological bot that piles resting Day orders on the book
    with no intention of trading.

    On each tick it submits [orders_per_tick] non-marketable orders spread
    over [symbols], quoting both sides: buys at
    [fundamental - price_offset_cents] and sells at
    [fundamental + price_offset_cents]. The offset keeps every order off the
    marketable price, so each one rests in the book rather than crossing the
    spread.

    The bot is purely order-generating and [on_event] is a no-op. Overall
    intensity is [orders_per_tick] times the tick rate configured in the
    bot's [Bot_spec]. *)

module Config : sig
  type t =
    { symbols : Symbol.t list (** symbols to pile resting orders across *)
    ; orders_per_tick : int
    (** orders submitted each tick aka the primary intensity knob *)
    ; order_size : Size.t (** shares per order; kept small *)
    ; price_offset_cents : int
    (** distance from the fundamental at which to price each order so it
        rests and doesn't get filled *)
    ; mutable next_client_id : int
    (** running counter to avoid duplicate-id rejection *)
    }
end

include Jsip_bot_runtime.Bot_runtime.Bot with module Config := Config
