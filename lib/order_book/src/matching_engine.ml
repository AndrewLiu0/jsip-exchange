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
  { books : Order_book.t array
  (** One book per symbol, indexed by {!Symbol_id.t}: the id on the wire is
      the array index, so resolving a symbol costs a bounds check plus an
      array read. (Exercise 2 kept a symbol->id hashtable here; the client
      now sends the id itself, so the hash step is gone entirely.) *)
  ; order_id_gen : Order_id.Generator.t
  ; mutable next_fill_id : int
  ; mutable client_order_id_to_order : Order.t Order_key.Map.t
  }
[@@deriving sexp_of]

let create ~num_symbols =
  { books =
      Array.init num_symbols ~f:(fun id ->
        Order_book.create (Symbol_id.of_int_exn id))
  ; order_id_gen = Order_id.Generator.create ()
  ; next_fill_id = 1
  ; client_order_id_to_order = Order_key.Map.empty
  }
;;

(* The book for [id], or [None] if this engine doesn't trade it. [id] arrives
   off the wire, where bin_io can deserialize ANY int — negative included —
   so this lookup is the server's whole defense against a malformed or
   hostile id: it must never let an out-of-range value reach the array index. *)
let find_book t (id : Symbol_id.t) : Order_book.t option =
  let books = t.books in
  let index = Symbol_id.to_int id in
  if index < 0 || index >= Array.length books
  then None
  else Some books.(index)
;;

let book t id = find_book t id

(** Run the matching loop: repeatedly find a compatible resting order and
    fill against it. Returns the list of Fill and Trade_report events
    produced, and the next fill_id to use. *)
let rec match_loop ~book ~order ~fill_id =
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
      (* Fully-filled orders leave the book but keep their slot in
         [client_order_id_to_order]: client order IDs are never reused, so a
         duplicate submission is rejected even after the original order is
         gone (see the duplicate check in [submit]). *)
      if Order.is_fully_filled resting
      then Order_book.remove book (Order.order_id resting);
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
        match_loop ~book ~order ~fill_id:(fill_id + 1)
      in
      fill_event :: trade_event :: remaining_events, next_fill_id)
;;

let submit t ~participant (request : Order.Request.t) =
  (* Preventing duplicate client_order_id *)
  if Map.mem t.client_order_id_to_order (participant, request.client_order_id)
  then
    [ Exchange_event.Order_reject
        { participant; request; reason = "duplicate client order id" }
    ]
  else (
    match find_book t request.symbol with
    | None ->
      [ Exchange_event.Order_reject
          { participant; request; reason = "unknown symbol" }
      ]
    | Some book ->
      let order_id = Order_id.Generator.next t.order_id_gen in
      let order = Order.create request ~order_id ~participant in
      let accepted =
        Exchange_event.Order_accept { order_id; participant; request }
      in
      (* Updating client order ID map *)
      let updated_map =
        Map.set
          t.client_order_id_to_order
          ~key:(participant, request.client_order_id)
          ~data:order
      in
      t.client_order_id_to_order <- updated_map;
      (* Snapshot BBO before matching so we can detect changes. *)
      let bbo_before = Order_book.best_bid_offer book in
      (* Match *)
      let fill_events, next_fill_id =
        match_loop ~book ~order ~fill_id:t.next_fill_id
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
                ; client_order_id = Order.client_order_id order
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

let order_not_found ~participant ~client_order_id =
  Exchange_event.Cancel_reject
    { participant; client_order_id; reason = "order not found" }
;;

let cancel
  t
  (participant : Participant.t)
  (client_order_id : Client_order_id.t)
  : Exchange_event.t list
  =
  let order_key = participant, client_order_id in
  match Map.find t.client_order_id_to_order order_key with
  | None -> [ order_not_found ~participant ~client_order_id ]
  | Some order ->
    (* Direct index, no bounds check: the id was validated by [find_book]
       when this order was submitted, and the book array never shrinks. *)
    let book = t.books.(Symbol_id.to_int (Order.symbol order)) in
    (match Order_book.find book (Order.order_id order) with
     | None ->
       (* The order was submitted under this [(participant, client_order_id)]
          at some point, but it's no longer in the book — either fully filled
          or previously cancelled. Both are reported as "not found" so the
          client can't distinguish them. *)
       [ order_not_found ~participant ~client_order_id ]
     | Some (_ : Order.t) ->
       let cancelled =
         Exchange_event.Order_cancel
           { order_id = Order.order_id order
           ; client_order_id = Order.client_order_id order
           ; participant = Order.participant order
           ; symbol = Order.symbol order
           ; remaining_size = Order.remaining_size order
           ; reason = Cancel_reason.Participant_requested
           }
       in
       (* BBO snapshot; the ID slot stays occupied so it can't be reused. *)
       let bbo_before = Order_book.best_bid_offer book in
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
