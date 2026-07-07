open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

module Config : sig
  type t =
    { symbol : Symbol.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; fill_client_oid : int ref
    ; mutable inventory : int Symbol.Map.t
    ; mutable currently_resting_orders : Size.t Client_order_id.Map.t
    }
  [@@deriving sexp_of]
end

val name : string
val on_start : Config.t -> Context.t -> unit Deferred.t
val on_tick : Config.t -> Context.t -> unit Deferred.t
val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t
