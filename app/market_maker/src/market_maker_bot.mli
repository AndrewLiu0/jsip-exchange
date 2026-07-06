(** A dynamic market maker on the {!Jsip_bot_runtime.Bot_runtime.Bot}
    interface.

    Unlike the static {!Market_maker.seed_book}, this bot reacts to its own
    fills. It keeps two pieces of state per instance:

    - an inventory counter (filled buys add, filled sells subtract), and
    - the set of client order ids it believes are resting on the book.

    On every fill involving this bot it cancels all outstanding orders and
    re-posts a fresh ladder of [num_levels] bids and asks around a {e skewed}
    fair value:

    {[
      skewed_fair = fundamental - (inventory * inventory_skew_cents_per_share)
    ]}

    so accumulating a long position pulls both quotes down (and a short
    position pushes them up), attracting the flow that brings inventory back
    toward zero. The fundamental comes from the runtime's oracle via
    {!Jsip_bot_runtime.Bot_runtime.Context.fundamental}; participant identity
    and the submit/cancel RPCs also come from the [Context], so the config
    carries only strategy parameters.

    Run it under [Jsip_scenario_runner.Runner] alongside other bots by
    listing it in a scenario's bot specs. *)

open! Core
open! Async
open Jsip_types

module Config : sig
  type t [@@deriving sexp_of]

  val create
    :  symbol:Symbol.t
    -> half_spread_cents:int
         (** The tightest bid rests at [skewed_fair - half_spread_cents] and
             the tightest ask at [skewed_fair + half_spread_cents]; each
             further level steps one cent away. *)
    -> size_per_level:int
    -> num_levels:int
    -> inventory_skew_cents_per_share:int
         (** How many cents the quoted fair value shifts per share of
             inventory. Too small and inventory grows unchecked; too large
             and the quotes drift out of the trading range entirely. *)
    -> t
end

include Jsip_bot_runtime.Bot_runtime.Bot with module Config := Config

module For_testing : sig
  (** Current net position, in shares. *)
  val inventory : Config.t -> int

  (** Client order ids currently believed resting, sorted. *)
  val outstanding : Config.t -> Client_order_id.t list

  (** Signed inventory change [fill] implies for participant [me]; [0] if
      [me] is not a party to the fill. *)
  val inventory_delta : Fill.t -> me:Participant.t -> int
end
