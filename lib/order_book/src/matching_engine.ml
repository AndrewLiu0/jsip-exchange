open! Core
open Jsip_types

module Order_key = struct
  module T = struct
    type t = Participant.t * Client_order_id.t
    [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

type t =
  { books : Order_book.t Symbol.Map.t
  ; order_id_gen : Order_id.Generator.t
  ; mutable next_fill_id : int
  ; mutable client_order_id_to_order : Order.t Order_key.Map.t
  }
[@@deriving sexp_of]

let create symbols =
  let books =
    List.map symbols ~f:(fun sym -> sym, Order_book.create sym)
    |> Symbol.Map.of_alist_exn
  in
  { books
  ; order_id_gen = Order_id.Generator.create ()
  ; next_fill_id = 1
  ; client_order_id_to_order = Order_key.Map.empty
  }
;;

let book t symbol = Map.find t.books symbol

(** Run the matching loop: repeatedly find a compatible resting order and
    fill against it. Returns the list of Fill and Trade_report events
    produced, and the next fill_id to use. *)
let rec match_loop t ~book ~order ~fill_id =
  if Size.( <= ) (Order.remaining_size order) Size.zero
  then [], fill_id
  else (
    match Order_book.find_match book order with
    | None -> [], fill_id
    | Some resting ->
      let fill_size =
        Size.min (Order.remaining_size order) (Order.remaining_size resting)
      in
      Order.fill order ~by:fill_size;
      Order.fill resting ~by:fill_size;
      if Order.is_fully_filled resting
      then Order_book.remove book (Order.order_id resting);
      (* TODO: do we also have to remove client_order_id here? *)
      let updated_map =
        Map.remove
          t.client_order_id_to_order
          (Order.participant resting, Order.client_order_id resting)
      in
      t.client_order_id_to_order <- updated_map;

      let fill_event =
        Exchange_event.Fill
          { fill_id
          ; symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          ; aggressor_client_order_id = Order.client_order_id order
          ; aggressor_order_id = Order.order_id order
          ; aggressor_participant = Order.participant order
          ; aggressor_side = Order.side order
          ; resting_order_id = Order.order_id resting
          ; resting_participant = Order.participant resting
          ; resting_client_order_id = Order.client_order_id resting
          }
      in
      let trade_event =
        Exchange_event.Trade_report
          { symbol = Order.symbol order
          ; price = Order.price resting
          ; size = fill_size
          }
      in
      let remaining_events, next_fill_id =
        match_loop t ~book ~order ~fill_id:(fill_id + 1)
      in
      fill_event :: trade_event :: remaining_events, next_fill_id)
;;

let submit t (request : Order.Request.t) =
  (* Preventing duplicate client_order_id *)
  if Map.mem
       t.client_order_id_to_order
       (request.participant, request.client_order_id)
  then
    [ Exchange_event.Order_reject
        { request; reason = "client order ID already in use" }
    ]
  else (
    match Map.find t.books request.symbol with
    | None ->
      [ Exchange_event.Order_reject { request; reason = "unknown symbol" } ]
    | Some book ->
      let order_id = Order_id.Generator.next t.order_id_gen in
      let order = Order.create request ~order_id in
      let accepted = Exchange_event.Order_accept { order_id; request } in
      (* Updating client order ID map *)
      let updated_map =
        Map.set
          t.client_order_id_to_order
          ~key:(request.participant, request.client_order_id)
          ~data:order
      in
      t.client_order_id_to_order <- updated_map;
      (* Snapshot BBO before matching so we can detect changes. *)
      let bbo_before = Order_book.best_bid_offer book in
      (* Match *)
      let fill_events, next_fill_id =
        match_loop t ~book ~order ~fill_id:t.next_fill_id
      in
      t.next_fill_id <- next_fill_id;
      (* Post-match: rest on book or cancel unfilled remainder. *)
      let post_events =
        if Size.( > ) (Order.remaining_size order) Size.zero
        then (
          match Order.time_in_force order with
          | Day ->
            Order_book.add book order;
            []
          | Ioc ->
            [ Exchange_event.Order_cancel
                { order_id
                ; participant = Order.participant order
                ; symbol = Order.symbol order
                ; remaining_size = Order.remaining_size order
                ; reason = Ioc_remainder
                }
            ])
        else []
      in
      (* Emit BBO update if the best bid or ask changed. *)
      let bbo_after = Order_book.best_bid_offer book in
      let bbo_events =
        if Bbo.equal bbo_before bbo_after
        then []
        else
          [ Exchange_event.Best_bid_offer_update
              { symbol = Order.symbol order; bbo = bbo_after }
          ]
      in
      List.concat [ [ accepted ]; fill_events; post_events; bbo_events ])
;;

let cancel
  t
  (participant : Participant.t)
  (client_order_id : Client_order_id.t)
  : Exchange_event.t list
  =
  let order_key = (participant, client_order_id) in
  match Map.find t.client_order_id_to_order order_key with
  | None ->
    [ Exchange_event.Cancel_reject
        { participant
        ; client_order_id
        ; reason = "client_order_id doesn't exist for participant"
        }
    ]
  | Some order ->
    (match Map.find t.books (Order.symbol order) with
     | None ->
       [ Exchange_event.Cancel_reject
           { participant; client_order_id; reason = "unknown symbol" }
       ]
     | Some book ->
       let cancelled =
         Exchange_event.Order_cancel
           { order_id = Order.order_id order
           ; participant = Order.participant order
           ; symbol = Order.symbol order
           ; remaining_size = Order.remaining_size order
           ; reason = Cancel_reason.Participant_requested
           }
       in
       (* BBO snapshot *)
       let bbo_before = Order_book.best_bid_offer book in
       (* Removal from client order id map AND actual remove from order_book *)
       let updated_map = Map.remove t.client_order_id_to_order order_key in
       t.client_order_id_to_order <- updated_map;
       Order_book.remove book (Order.order_id order);
       let bbo_after = Order_book.best_bid_offer book in
       let bbo_events =
         if Bbo.equal bbo_before bbo_after
         then []
         else
           [ Exchange_event.Best_bid_offer_update
               { symbol = Order.symbol order; bbo = bbo_after }
           ]
       in
       List.concat [ [ cancelled ]; bbo_events ])
;;
