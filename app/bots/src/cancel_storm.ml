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

    Driven by the [Cancel_storm] scenario in {!Jsip_scenarios.Cancel_storm}. *)

open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime

module Config = struct
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

let name = "cancel-storm"

(* No one-time setup needed: the storm is entirely tick-driven. *)
let on_start (_ : Config.t) (_ : Bot_runtime.Context.t) : unit Deferred.t =
  return ()
;;

let passive_price (config : Config.t) (ctx : Bot_runtime.Context.t) =
  let fundamental = Bot_runtime.Context.fundamental ctx config.symbol in
  let offset = Price.of_int_cents config.offset_from_fundamental_cents in
  match config.side with
  | Buy -> Price.(fundamental - offset)
  | Sell -> Price.(fundamental + offset)
;;

let submit_and_cancel_one (config : Config.t) (ctx : Bot_runtime.Context.t) =
  let client_order_id = Client_order_id.of_int !(config.next_id) in
  incr config.next_id;
  let request : Order.Request.t =
    { client_order_id
    ; symbol = config.symbol
    ; side = config.side
    ; price = passive_price config ctx
    ; size = config.size
    ; time_in_force = Day
    }
  in
  let%bind submit_result = Bot_runtime.Context.submit ctx request in
  (match submit_result with
   | Ok () -> ()
   | Error error ->
     [%log.error
       "cancel_storm: submit failed"
         (request : Order.Request.t)
         (error : Error.t)]);
  let%bind cancel_result = Bot_runtime.Context.cancel ctx client_order_id in
  (match cancel_result with
   | Ok () -> ()
   | Error error ->
     [%log.error
       "cancel_storm: cancel failed"
         (client_order_id : Client_order_id.t)
         (error : Error.t)]);
  return ()
;;

let on_tick (config : Config.t) (ctx : Bot_runtime.Context.t)
  : unit Deferred.t
  =
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.pairs_per_tick ~f:Fn.id)
    ~f:(fun (_ : int) -> submit_and_cancel_one config ctx)
;;

(* Purely observational: cancels are fired blind (see module doc), so this
   bot has no bookkeeping to do in response to events. Logging rejections
   makes it easy to see the storm's effect on the exchange while a scenario
   is running. *)
let on_event
  (_ : Config.t)
  (_ : Bot_runtime.Context.t)
  (event : Exchange_event.t)
  : unit Deferred.t
  =
  (match event with
   | Order_reject { participant; request; reason } ->
     [%log.info
       "cancel_storm: order rejected"
         (participant : Participant.t)
         (request : Order.Request.t)
         (reason : string)]
   | Cancel_reject { participant; client_order_id; reason } ->
     [%log.info
       "cancel_storm: cancel rejected"
         (participant : Participant.t)
         (client_order_id : Client_order_id.t)
         (reason : string)]
   | Order_accept _ | Order_cancel _ | Fill _ | Best_bid_offer_update _
   | Trade_report _ ->
     ());
  return ()
;;
