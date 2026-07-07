open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

module Config = struct
  type t =
    { symbol : Symbol.t
    ; order_size : int
    ; price_jitter_cents : int
    ; next_client_order_id : int ref
    }
  [@@deriving sexp_of]
end

let name = "tick_trader"
let on_start (_config : Config.t) (_context : Context.t) = return ()

let on_event
  (_config : Config.t)
  (_context : Context.t)
  (_event : Exchange_event.t)
  =
  return ()
;;

let on_tick (config : Config.t) (context : Context.t) =
  let rng = Context.random context in
  let client_order_id =
    Client_order_id.of_int !(config.next_client_order_id)
  in
  incr config.next_client_order_id;
  let side : Side.t =
    if Splittable_random.int rng ~lo:0 ~hi:1 = 0 then Buy else Sell
  in
  let base_cents =
    Price.to_int_cents (Context.fundamental context config.symbol)
  in
  let jitter =
    Splittable_random.int
      rng
      ~lo:(-config.price_jitter_cents)
      ~hi:config.price_jitter_cents
  in
  let request : Order.Request.t =
    { client_order_id
    ; symbol = config.symbol
    ; side
    ; price = Price.of_int_cents (base_cents + jitter)
    ; size = Size.of_int config.order_size
    ; time_in_force = Day
    }
  in
  match%map Context.submit context request with
  | Ok () -> ()
  | Error error ->
    [%log.error "tick_trader: submit failed" (error : Error.t)]
;;
