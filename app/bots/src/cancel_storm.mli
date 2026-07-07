(** A pathological bot: submits an order, immediately cancels it, and repeats
    forever.

    This bot is deliberately adversarial. It targets three things at once:

    - The cancel path itself ([cancel_order_rpc] /
      {!Matching_engine.cancel}).
    - The submit/accept/cancel event flow on the bot's own session-feed pipe
      — every pair produces (at least) an [Order_accept] and an
      [Order_cancel], back to back.
    - The duplicate-client-order-id bookkeeping from Part 2. Each order uses
      a fresh [Client_order_id.t] that is never reused, because
      {!Matching_engine}'s [orders_by_client_id] table
      (lib/order_book/src/matching_engine.ml) never frees an entry — not on
      cancel, not on fill. A storm that runs forever grows that table
      forever, one entry per order, even though every order it produced is
      long gone from the book. That unbounded growth (not the cancel RPC
      itself) is the real thing to watch on a dashboard.

    Cancels are fired immediately after submitting, without waiting for the
    [Order_accept] to come back: {!Bot_runtime.Context.cancel} takes the
    client-chosen [Client_order_id.t], which this bot already knows the
    moment it builds the request, so there is nothing to wait for. This
    mirrors what a real misbehaving client can do.

    Driven by the [Cancel_storm] scenario in {!Jsip_scenarios.Cancel_storm}.
    Satisfies {!Jsip_bot_runtime.Bot_runtime.Bot}, so it plugs into
    {!Jsip_scenario_runner.Bot_spec.t} the same way
    {!Jsip_market_maker.Market_maker} does. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime

(** Configuration for one storm instance. Exposed as a concrete record (not
    abstract) because scenarios construct it directly with record syntax —
    see [app/scenarios/src/cancel_storm.ml]. *)
module Config : sig
  type t =
    { symbol : Symbol.t
    ; size : Size.t
    ; side : Side.t
    (** Side to submit on. Launch two instances (one [Buy], one [Sell]) for a
        storm that hits both sides of the book. *)
    ; offset_from_fundamental_cents : int
    (** Distance from the fundamental price, on the passive side, so orders
        rest instead of crossing the spread and filling before they can be
        cancelled. A [Buy] prices at [fundamental - offset]; a [Sell] prices
        at [fundamental + offset]. *)
    ; pairs_per_tick : int
    (** How many submit/cancel pairs to fire per tick. *)
    ; next_id : int ref
    (** Monotonically increasing counter for this instance's client order
        ids. Never reused — see the module doc for why that matters. Each
        [Config.t] must own its own ref; two instances sharing one would
        collide on ids. *)
    }
  [@@deriving sexp_of]
end

(** ["cancel-storm"]. *)
val name : string

(** No one-time setup needed: the storm is entirely tick-driven. *)
val on_start : Config.t -> Bot_runtime.Context.t -> unit Deferred.t

(** Fires [config.pairs_per_tick] submit/cancel pairs in parallel. *)
val on_tick : Config.t -> Bot_runtime.Context.t -> unit Deferred.t

(** Purely observational: cancels are fired blind (see module doc), so this
    bot has no bookkeeping to do in response to events. Logs rejections so
    the storm's effect on the exchange is visible while a scenario runs. *)
val on_event
  :  Config.t
  -> Bot_runtime.Context.t
  -> Exchange_event.t
  -> unit Deferred.t
