open! Core
open! Async
open Jsip_types
open Jsip_gateway
open Jsip_bot_runtime.Bot_runtime

module Config = struct
  type t =
    { symbol : Symbol.t
    ; participant: Participant.t
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    }
  [@@deriving sexp_of]
end

let seed_book (config : Config.t) conn =
  let gen = Client_order_id.Generator.create () in
  let submit request =
    let%map result =
      Rpc.Rpc.dispatch_exn Rpc_protocol.submit_order_rpc conn request
    in
    match result with
    | Ok () -> ()
    | Error msg ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (msg : Error.t)]
  in
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let%bind () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Buy
           ; price = Price.of_int_cents (config.fair_value_cents - offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = Client_order_id.Generator.next gen
           }
           : Order.Request.t)
      and () =
        submit
          ({ symbol = config.symbol
           ; participant = config.participant
           ; side = Sell
           ; price = Price.of_int_cents (config.fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           ; client_order_id = Client_order_id.Generator.next gen
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

let run (config : Config.t) conn : unit Deferred.t =
  let%bind () = seed_book config conn in
  let%bind result =
    Rpc.Pipe_rpc.dispatch Rpc_protocol.session_feed_rpc conn ()
  in
  let reader =
    match result with
    | Ok (Ok (reader, _id)) -> reader
    | _ -> failwith "subscribe session failed"
  in
  don't_wait_for
    (Pipe.iter_without_pushback reader ~f:(fun event ->
       let e = Protocol.format_event event in
       print_endline [%string "[for MarketMaker] %{e}"]));
  return ()
;;

module Market_maker_bot = struct
  module Config = struct
    type t =
      { symbol : Symbol.t
      ; half_spread : float
      ; order_size : int
      }
  end

  let name = "market_maker_bot"
  let on_start (_config : Config.t) (_ctx:Context.t) = return ()
  let on_tick _ _ctx = return ()
  let on_event (_config : Config.t) (_ctx:Context.t) event = return ()
end
