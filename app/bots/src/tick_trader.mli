open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

(* Not a pathological bot. Small noise trader to generate market-data traffic
   for Slow_consumers bot scenario.

   Each tick, it submits one resting Day order on a random side at a price
   jittered around the current fundamental. Will swap this out for a real
   noise trader when group is done.

   The rate depends only on the tick interval. *)

module Config : sig
  type t =
    { symbol : Symbol.t (** The symbol to trade. *)
    ; order_size : int
    ; price_jitter_cents : int
    (** Each order is priced at [fundamental +/- U(0, price_jitter_cents)].
        Larger values spread orders over more price levels, resulting in more
        BBO changes. *)
    ; next_client_order_id : int ref
    }
  [@@deriving sexp_of]
end

val name : string
val on_start : Config.t -> Context.t -> unit Deferred.t
val on_tick : Config.t -> Context.t -> unit Deferred.t
val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t
