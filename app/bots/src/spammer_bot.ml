open! Core
open! Async
open Jsip_bot_runtime
open Jsip_types

module SpammerConfig = struct
  type t =
    { participant : Participant.t
    ; symbols : Symbol.t list
    ; side : Side.t
    ; burst_interval : int
    ; burst_size : int
    ; dist_from_fundamental_cents : int
    ; order_size : int
    ; client_order_id_generator : Client_order_id.Generator.t
    ; mutable ticks_since_prev_burst : int
    }
end

module T = struct
  module Config = struct
    type t = SpammerConfig.t
  end

  let name = "Spammer"

  (* The resting price for one spam order. A passive order must sit on the
     far side of the fundamental so it does not cross the book and trade
     immediately: a resting Buy sits [dist_cents] BELOW the fundamental, a
     resting Sell sits [dist_cents] ABOVE it. The larger [dist_cents], the
     deeper in the book the order rests and the longer it lingers. *)
  let passive_price
    ~(fundamental : Price.t)
    ~(side : Side.t)
    ~(dist_cents : int)
    : Price.t
    =
    let dist_price = Price.of_int_cents dist_cents in
    match side with
    | Buy -> Price.( - ) fundamental dist_price
    | Sell -> Price.( + ) fundamental dist_price
  ;;

  (* Build and send one order for [symbol] on the configured side, resting
     the configured distance from the fundamental, with a fresh client order
     id. *)
  let send_one_order
    (config : Config.t)
    (context : Bot_runtime.Context.t)
    symbol
    =
    let fundamental = Bot_runtime.Context.fundamental context symbol in
    let request : Order.Request.t =
      { symbol
      ; side = config.side
      ; price =
          passive_price
            ~fundamental
            ~side:config.side
            ~dist_cents:config.dist_from_fundamental_cents
      ; size = Size.of_int config.order_size
      ; time_in_force = Day
      ; client_order_id =
          Client_order_id.Generator.generate config.client_order_id_generator
      }
    in
    (* The bot is adversarial: it does not care whether any individual
       request is accepted or rejected, so the submit result is intentionally
       dropped. *)
    let%map (_ : unit Or_error.t) =
      Bot_runtime.Context.submit context request
    in
    ()
  ;;

  (* Fire a single burst: send [burst_size] orders to every configured
     symbol. *)
  let fire_burst (config : Config.t) (context : Bot_runtime.Context.t) =
    Deferred.List.iter config.symbols ~how:`Parallel ~f:(fun symbol ->
      Deferred.List.iter
        (List.init config.burst_size ~f:Fn.id)
        ~how:`Parallel
        ~f:(fun _ -> send_one_order config context symbol))
  ;;

  let on_start (config : Config.t) (_context : Bot_runtime.Context.t) =
    assert (config.burst_interval >= 1);
    assert (config.burst_size >= 0);
    assert (config.dist_from_fundamental_cents >= 0);
    assert (config.order_size >= 1);
    config.ticks_since_prev_burst <- 0;
    return ()
  ;;

  let on_event
    (_config : Config.t)
    (_context : Bot_runtime.Context.t)
    (_event : Exchange_event.t)
    =
    return ()
  ;;

  let on_tick (config : Config.t) (context : Bot_runtime.Context.t) =
    config.ticks_since_prev_burst <- config.ticks_since_prev_burst + 1;
    if config.ticks_since_prev_burst >= config.burst_interval
    then (
      let%map () = fire_burst config context in
      config.ticks_since_prev_burst <- 0)
    else return ()
  ;;
end

include T
