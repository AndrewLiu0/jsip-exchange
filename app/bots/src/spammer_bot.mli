open! Core
open Jsip_types
open Jsip_bot_runtime

(* Description *)
(* This bot is an adversarial bot that targets the request queue of the
   server, attempting to use up resources by sending a fixed burst of orders
   every [burst_interval] ticks. Its behavior is fully deterministic: every
   order rests [dist_from_fundamental_cents] away from the fundamental (a Buy
   below, a Sell above), so a larger distance leaves more orders sitting on
   the book rather than trading away. *)
module SpammerConfig : sig
  type t =
    { participant : Participant.t (* Bot's participant name *)
    ; symbols : Symbol.t list (* Books that the bot is targeting *)
    ; side : Side.t (* the side every order is sent on *)
    ; burst_interval : int (* number of ticks between consecutive bursts *)
    ; burst_size : int (* number of orders sent to each symbol per burst *)
    ; dist_from_fundamental_cents : int
        (* distance from the fundamental value in cents; a Buy rests this far
           below the fundamental, a Sell this far above *)
    ; order_size : int
        (* shares per order; does not change how many orders rest on the
           book, only their size, since each request becomes exactly one
           order *)
    ; client_order_id_generator : Client_order_id.Generator.t
        (* supplies a fresh client order id for every request the bot sends *)
    ; mutable ticks_since_prev_burst : int
    }
end

include Bot_runtime.Bot with type Config.t = SpammerConfig.t
