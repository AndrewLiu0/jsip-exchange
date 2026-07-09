open! Core
open Jsip_types
module Order_queue = Hash_queue.Make (Order_id)

type t =
  { symbol : Symbol.t
  ; mutable bids : Order.t Order_queue.t Price.Map.t
  ; mutable asks : Order.t Order_queue.t Price.Map.t
  ; mutable id_to_order : Order.t Order_id.Map.t (*Can make Hashtable? *)
  }
[@@deriving sexp_of]

let create symbol =
  { symbol
  ; bids = Price.Map.empty
  ; asks = Price.Map.empty
  ; id_to_order = Order_id.Map.empty
  }
;;

let symbol t = t.symbol

let side_map t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side_map t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

(* Unused but needed for interface *)
let orders_on_side t side =
  List.filter (Map.data t.id_to_order) ~f:(fun order ->
    Side.equal (Order.side order) side)
;;

let add t order =
  let price = Order.price order in
  let side = Order.side order in
  let side_map = side_map t side in
  let order_id = Order.order_id order in
  t.id_to_order <- Map.set t.id_to_order ~key:order_id ~data:order;
  let find_result = Map.find side_map price in
  let queue =
    match find_result with
    | Some q -> q
    | None ->
      let q = Order_queue.create () in
      let updated_side_map = Map.set side_map ~key:price ~data:q in
      set_side_map t side updated_side_map;
      q
  in
  Order_queue.enqueue_back_exn queue order_id order
;;

let get_queue t order_id =
  let open Option.Let_syntax in
  let%bind order = Map.find t.id_to_order order_id in
  let side_map = side_map t (Order.side order) in
  Map.find side_map (Order.price order)
;;

let remove' t order_id : Order.t option =
  let open Option.Let_syntax in
  let%bind queue = get_queue t order_id in
  let%bind order = Map.find t.id_to_order order_id in
  let result = Order_queue.lookup_and_remove queue order_id in
  let new_id_to_order = Map.remove t.id_to_order order_id in
  t.id_to_order <- new_id_to_order;
  if Order_queue.length queue = 0
  then (
    let side = Order.side order in
    let side_map = side_map t side in
    let new_map = Map.remove side_map (Order.price order) in
    set_side_map t side new_map);
  result
;;

let remove t order_id : unit = ignore (remove' t order_id)

let find t order_id =
  let open Option.Let_syntax in
  let%bind queue = get_queue t order_id in
  Order_queue.lookup queue order_id
;;

let find_best_order t side =
  let resting_orders = side_map t side in
  let best =
    match side with
    | Buy -> Map.max_elt resting_orders
    | Sell -> Map.min_elt resting_orders
  in
  let open Option.Let_syntax in
  let%bind _price, best_queue = best in
  Order_queue.first best_queue
;;

let find_match t incoming : Order.t option =
  let incoming_side = Order.side incoming in
  let opposite_side = Side.flip incoming_side in
  let open Option.Let_syntax in
  let%bind best_resting_order = find_best_order t opposite_side in
  if Price.is_marketable
       incoming_side
       ~price:(Order.price incoming)
       ~resting_price:(Order.price best_resting_order)
  then Some best_resting_order
  else None
;;

let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks
let count t side = Map.length (side_map t side)

let best_price t side =
  let best_order = find_best_order t side in
  match best_order with
  | Some order -> Some (Order.price order)
  | None -> None
;;

let queue_to_level t (side : Side.t) (price : Price.t) : Level.t =
  let queue = Map.find_exn (side_map t side) price in
  let total_size =
    Order_queue.fold queue ~init:Size.zero ~f:(fun acc order ->
      Size.( + ) acc (Order.remaining_size order))
  in
  { Level.price; size = total_size }
;;

let best_level t side : Level.t option =
  let best_price = best_price t side in
  match best_price with
  | None -> None
  | Some price ->
    let best_level = queue_to_level t side price in
    Some best_level
;;

let best_bid_offer t : Bbo.t =
  { bid = best_level t Buy; ask = best_level t Sell }
;;

let snapshot_side t (side : Side.t) =
  let map_on_side = side_map t side in
  let level_map =
    Map.mapi map_on_side ~f:(fun ~key:price ~data:_hashqueue ->
      queue_to_level t side price)
  in
  let level_list = Map.data level_map in
  if Side.equal side Side.Buy then List.rev level_list else level_list
;;

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  let remove = remove'
end
