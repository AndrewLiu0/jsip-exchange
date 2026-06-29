open! Core

type t =
  { fill_id : int
  ; symbol : Symbol.t
  ; price : Price.t
  ; size : Size.t
  ; aggressor_client_order_id: Client_order_id.t
  ; aggressor_order_id : Order_id.t
  ; aggressor_participant : Participant.t
  ; aggressor_side : Side.t
  ; resting_client_order_id: Client_order_id.t
  ; resting_order_id : Order_id.t
  ; resting_participant : Participant.t
  }
[@@deriving sexp, bin_io]

let to_string
  ({ fill_id
   ; symbol
   ; price
   ; size
   ; aggressor_client_order_id
   ; aggressor_order_id
   ; aggressor_participant
   ; aggressor_side
   ; resting_client_order_id
   ; resting_order_id
   ; resting_participant
   } :
    t)
  =
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s|%s(%s) %s resting=%s|%s(%s)"
    fill_id
    (Symbol.to_string symbol)
    (Price.to_string_dollar price)
    (Size.to_int size)
    (Client_order_id.to_string aggressor_client_order_id)
    (Order_id.to_string aggressor_order_id)
    (Participant.to_string aggressor_participant)
    (Side.to_string aggressor_side)
    (Client_order_id.to_string resting_client_order_id)
    (Order_id.to_string resting_order_id)
    (Participant.to_string resting_participant)
;;

let to_participant_view t participant =
  let resting_side = Side.flip t.aggressor_side in
  let resting_verb =
    match resting_side with Buy -> "bought" | Side.Sell -> "sold"
  in
  if Participant.equal t.resting_participant participant
  then
    Some
      (sprintf
         "You %s %d %s at %s"
         resting_verb
         (Size.to_int t.size)
         (Symbol.to_string t.symbol)
         (Price.to_string_dollar t.price))
  else None
;;

let notional_cents t = Price.to_int_cents t.price * Size.to_int t.size
