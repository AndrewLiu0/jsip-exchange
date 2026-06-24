open! Core
open Jsip_types
(* open Async_log_kernel.Ppx_log_syntax *)


module Order_queue = Hash_queue.Make(Order_id)
type t =
  { symbol : Symbol.t
  ; mutable bids : (Order.t Order_queue.t) Price.Map.t
  ; mutable asks : (Order.t Order_queue.t) Price.Map.t
  ; mutable id_to_order: Order.t Order_id.Map.t
  (*Lazy Removal for log n optimization*)
  }
[@@deriving sexp_of]

let create symbol = { 
  symbol; 
  bids = Price.Map.empty ;
  asks = Price.Map.empty; 
  id_to_order = Order_id.Map.empty
  }
let symbol t = t.symbol

let side_map t side =
  match (side : Side.t) with Buy -> t.bids | Sell -> t.asks
;;

let set_side_map t side orders =
  match (side : Side.t) with
  | Buy -> t.bids <- orders
  | Sell -> t.asks <- orders
;;

let add t order =
  let price = Order.price order in
  let side = Order.side order in
  let side_map = side_map t side in 
  let order_id = Order.order_id order in
  (*Creating new map and updating old map*)
  t.id_to_order <- Map.set t.id_to_order ~key:order_id ~data:order;


  let find_result = Map.find side_map price in 
  let queue = match find_result with 
  | Some q -> q
  | None -> Order_queue.create() in
  ignore(Order_queue.enqueue_back queue order_id order )
;;

let get_queue t order_id = 
  let order = Map.find_exn t.id_to_order order_id in 
  let side_map = side_map t (Order.side order) in
  Map.find_exn side_map (Order.price order)



let remove' t order_id: Order.t option= 
  let queue = get_queue t order_id in
  let result = Order_queue.lookup_and_remove queue order_id in

  (* Cleanup from order_book data structures*)
  let new_id_to_order =  Map.remove t.id_to_order order_id in
  t.id_to_order <- new_id_to_order;
  (if Order_queue.length queue = 0
    then 
      let order = Map.find_exn t.id_to_order order_id in
      let side = Order.side order in 
      let side_map = side_map t side in 
      let new_map = Map.remove side_map (Order.price order) in
      set_side_map t side new_map);
  result
;;

let remove t order_id: unit = 
  ignore (remove' t order_id)

let find t order_id =
  let queue = get_queue t order_id in
  Order_queue.lookup queue order_id
;;

(* let create_comparator side order_1 order_2 =
  let price_1 = Order.price order_1 in
  let price_2 = Order.price order_2 in
  let time_1 = Order.order_id order_1 in
  let time_2 = Order.order_id order_2 in
  if Price.is_more_aggressive side ~price:price_1 ~than:price_2
  then -1
  else if Price.is_more_aggressive side ~price:price_2 ~than:price_1
  then 1
  else if Order_id.(time_1 < time_2)
  then -1
  else 1
;; *)

let find_best_order t side = 

  let resting_orders = side_map t side in

  let best = match side with 
  | Buy -> Map.max_elt resting_orders 
  | Sell -> Map.min_elt resting_orders in 

  let open Option.Let_syntax in
  let %bind (_price, best_queue) = best in 
  Order_queue.dequeue_front best_queue



(* NOTE: This walks the list front-to-back and returns the *first* tradable
   order, not the best-priced one. Orders are in reverse insertion order
   (newest first), so this matches against whatever was most recently added,
   regardless of price. See test_matching_engine.ml for a test that
   demonstrates why this is wrong. *)
let find_match t incoming: Order.t option =
  let incoming_side = Order.side incoming in
  let opposite_side = Side.flip incoming_side in
  let open Option.Let_syntax in
  let %bind best_resting_order = find_best_order t opposite_side in
  if Price.is_marketable incoming_side ~price:(Order.price incoming) ~resting_price:(Order.price best_resting_order) 
    then Some best_resting_order else None

;;

let orders_on_side t side = List.filter (Map.data t.id_to_order) ~f:(fun order -> Side.equal (Order.side order) side )
let is_empty t = Map.is_empty t.bids && Map.is_empty t.asks
let count t side = Map.length (side_map t side)

let best_price t side =
  let best_order = find_best_order t side in 
  match best_order with 
  | Some order -> Some (Order.price order)
  | None -> None
;;

let queue_to_level t (side: Side.t) (price: Price.t) : Level.t = 
  let queue = Map.find_exn (side_map t side) price in
  let total_size = (Order_queue.fold queue ~init:Size.zero ~f:(fun acc order -> Size.(+) acc (Order.remaining_size order))) in 
  {Level.price = price; size = total_size}

  (* queue = Map.find_exn (side_map t side) price *)

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


(* TODO: ordering of level_list?? *)
let snapshot_side t (side : Side.t) =
  let map_on_side = side_map t side in
  let level_map = Map.mapi map_on_side ~f:(fun ~key:price ~data: _hashqueue -> queue_to_level t side price ) in 
  let level_list = Map.data level_map in
  level_list
;;

let snapshot t =
  { Book.symbol = symbol t
  ; bids = snapshot_side t Buy
  ; asks = snapshot_side t Sell
  ; bbo = best_bid_offer t
  }
;;

module For_testing = struct
  (* let remove = remove *)
  let remove = remove'
end
