open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

module Config = struct
  type t =
    { symbol : Symbol.t (* ----Need a market maker for each symbol *)
    ; fair_value_cents : int
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; fill_client_oid : int ref
        (* ----Don't need because we manually compute based on which position
           on the ladder we are posting *)
    ; mutable inventory : int Symbol.Map.t
    ; mutable currently_resting_orders : Size.t Client_order_id.Map.t
    }
  [@@deriving sexp_of]
end

let name = "market_maker"

let on_start (config : Config.t) (context : Context.t) =
  let submit request =
    let%map result = Context.submit context request in
    match result with
    | Ok () -> ()
    | Error msg ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (msg : Error.t)]
  in
  (* Assign each side at each level a distinct client order ID: level [L]
     gets [2*L + 99] for the bid and [2*L + 100] for the ask. Level one
     assigns IDs 101 and 102. *)
  Deferred.List.iter
    ~how:`Parallel
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level_idx ->
      let offset = config.half_spread_cents + level_idx in
      let bid_client_order_id =
        Client_order_id.of_int ((2 * level_idx) + 101)
      in
      let ask_client_order_id =
        Client_order_id.of_int ((2 * level_idx) + 102)
      in
      let%bind () =
        submit
          ({ client_order_id = bid_client_order_id
           ; symbol = config.symbol
           ; side = Buy
           ; price = Price.of_int_cents (config.fair_value_cents - offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           }
           : Order.Request.t)
      and () =
        submit
          ({ client_order_id = ask_client_order_id
           ; symbol = config.symbol
           ; side = Sell
           ; price = Price.of_int_cents (config.fair_value_cents + offset)
           ; size = Size.of_int config.size_per_level
           ; time_in_force = Day
           }
           : Order.Request.t)
      in
      Deferred.unit)
;;

let on_tick _config _context = return ()

let remove_resting_order (config : Config.t) client_oid =
  config.currently_resting_orders
  <- Map.remove config.currently_resting_orders client_oid
;;

let add_resting_order (config : Config.t) client_oid size =
  config.currently_resting_orders
  <- Map.add_exn config.currently_resting_orders ~key:client_oid ~data:size
;;

let add_size_to_symbol (config : Config.t) symbol (signed_size : int) =
  config.inventory
  <- Map.update config.inventory symbol ~f:(function
       | None -> signed_size
       | Some curr -> curr + signed_size)
;;

let update_resting_order_size (config : Config.t) client_oid (size : Size.t) =
  let size = Size.to_int size in
  config.currently_resting_orders
  <- (match Map.find config.currently_resting_orders client_oid with
      | Some remaining_size ->
        let remaining_size = Size.to_int remaining_size in
        let updated_size = Size.of_int (remaining_size - size) in
        Map.change config.currently_resting_orders client_oid ~f:(function
          | Some _ -> Some updated_size
          | None -> None)
      | None ->
        [%log.error
          "Couldn't find that order in config.currently_resting_orders"];
        config.currently_resting_orders)
;;

let on_event (config : Config.t) (context : Context.t) event =
  let participant = Context.participant context in
  (match event with
   | Exchange_event.Order_accept { request; order_id = _; participant = _ }
     ->
     add_resting_order config request.client_order_id request.size
   | Order_cancel
       { client_order_id
       ; participant = _
       ; symbol = _
       ; remaining_size = _
       ; reason = _
       ; order_id = _
       } ->
     remove_resting_order config client_order_id
   | Fill
       { fill_id = _
       ; symbol
       ; price = _
       ; size
       ; aggressor_order_id = _
       ; aggressor_participant = _
       ; aggressor_side
       ; aggressor_client_order_id
       ; resting_order_id = _
       ; resting_participant
       ; resting_client_order_id
       } ->
     let side = ref aggressor_side in
     let signed_size = ref (Size.to_int size) in
     let client_oid = ref aggressor_client_order_id in
     if Participant.equal resting_participant participant
     then (
       side := Side.flip aggressor_side;
       client_oid := resting_client_order_id);
     (* Assume that market maker is on one side of the fill. We don't check
        that it is on the other side if it is not on the resting side. *)
     (match !side with Sell -> signed_size := !signed_size * -1 | Buy -> ());
     (* Update inventory with added size for symbol. *)
     add_size_to_symbol config symbol !signed_size;
     (* Update the size of currently resting orders - removes however much
        the fill sold/bought against us. *)
     update_resting_order_size config !client_oid size;
     (* Remove the corresponding resting order if the fill consumed the full
        remaining size of the order *)
     (match Map.find config.currently_resting_orders !client_oid with
      | None -> ()
      | Some size ->
        if Size.to_int size = 0
        then
          config.currently_resting_orders
          <- Map.remove config.currently_resting_orders !client_oid)
   | Order_reject _ | Cancel_reject _ | Best_bid_offer_update _
   | Trade_report _ ->
     ());
  Deferred.unit
;;
