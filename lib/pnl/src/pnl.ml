open! Core
open Jsip_types

module Trade_report = struct
  type t =
    { symbol : Symbol_id.t
    ; price : Price.t
    ; size : Size.t
    }
  [@@deriving sexp_of]

  let create ~symbol ~price ~size = { symbol; price; size }

  let of_exchange_event : Exchange_event.t -> t option = function
    | Trade_report { symbol; price; size } -> Some { symbol; price; size }
    | Order_accept _ | Fill _ | Order_cancel _ | Order_reject _
    | Best_bid_offer_update _ | Cancel_reject _ ->
      None
  ;;
end

module Position = struct
  type t =
    { shares : int
    ; average_entry_cents : float
    ; realized_cents : float
    }
  [@@deriving sexp_of, fields ~getters]

  let flat = { shares = 0; average_entry_cents = 0.; realized_cents = 0. }
end

(* Apply a single participant's execution (one side of a fill) to their
   position in one symbol, using average-cost accounting.

   For a trade of [size] shares at [price] on [side]:

   - {!Side.sign} gives the signed change to [shares]: buys are [+], sells
     are [-].
   - Growing the position (trading in the same direction, or opening from
     flat) blends [price] into [average_entry_cents], weighted by the shares
     on each side. The blended average is what later reductions are measured
     against.
   - Reducing or closing realizes cash on the closed shares:
     [closed * (price - average_entry_cents)] for a long, and the mirror
     image for a short. If the trade crosses through zero, the leftover
     shares open a fresh position at [price], and the average entry resets to
     [price]. When the position returns exactly to flat, the average entry is
     meaningless — reset it to [0.].

   Work in cents throughout; convert to dollars only at the {!Summary}
   boundary. *)
let apply_execution
  (position : Position.t)
  ~(side : Side.t)
  ~(price : Price.t)
  ~(size : Size.t)
  : Position.t
  =
  let { Position.shares = shares_before
      ; average_entry_cents = average_before
      ; realized_cents = realized_before
      }
    =
    position
  in
  let quantity = Size.to_int size in
  let trade_price = Float.of_int (Price.to_int_cents price) in
  let shares_after = shares_before + (Side.sign side * quantity) in
  (* A buy extends a position we are not short in; a sell extends a position
     we are not long in. Anything else trades against the position. *)
  let extends_position =
    match side with Buy -> shares_before >= 0 | Sell -> shares_before <= 0
  in
  if extends_position
  then (
    (* We are adding shares in the direction we already lean, so nothing is
       realized. Fold the new shares into the running average, weighting each
       side by its share count. *)
    let cost_of_shares_held =
      Float.of_int (abs shares_before) *. average_before
    in
    let cost_of_shares_added = Float.of_int quantity *. trade_price in
    let average_after =
      (cost_of_shares_held +. cost_of_shares_added)
      /. Float.of_int (abs shares_after)
    in
    { Position.shares = shares_after
    ; average_entry_cents = average_after
    ; realized_cents = realized_before
    })
  else (
    (* We are trading against the position, so we close shares and book their
       profit. In this branch a buy means we were short and a sell means we
       were long. *)
    let shares_closed = Int.min quantity (abs shares_before) in
    let were_long = shares_before > 0 in
    let profit_per_share =
      if were_long
      then
        trade_price -. average_before (* sold above our average = profit *)
      else average_before -. trade_price (* bought back below it = profit *)
    in
    let realized_after =
      realized_before +. (Float.of_int shares_closed *. profit_per_share)
    in
    let average_after =
      if shares_after = 0
      then 0. (* closed out completely: no position, no average *)
      else if shares_closed < quantity
      then
        trade_price
        (* closed through zero: the leftover shares open here *)
      else average_before (* only partly closed: the average is untouched *)
    in
    { Position.shares = shares_after
    ; average_entry_cents = average_after
    ; realized_cents = realized_after
    })
;;

type t =
  { positions : Position.t Symbol_id.Map.t Participant.Map.t
  ; references : Price.t Symbol_id.Map.t
  }
[@@deriving sexp_of]

let empty =
  { positions = Participant.Map.empty; references = Symbol_id.Map.empty }
;;

let position t participant symbol =
  Map.find t.positions participant
  |> Option.bind ~f:(fun by_symbol -> Map.find by_symbol symbol)
  |> Option.value ~default:Position.flat
;;

let set_position t participant symbol position =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol_id.Map.empty
  in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:position in
  { t with positions = Map.set t.positions ~key:participant ~data:by_symbol }
;;

let apply_one t ~participant ~symbol ~side ~price ~size =
  let updated =
    apply_execution (position t participant symbol) ~side ~price ~size
  in
  set_position t participant symbol updated
;;

let apply_fill t (fill : Fill.t) =
  let { Fill.symbol
      ; price
      ; size
      ; aggressor_participant
      ; aggressor_side
      ; resting_participant
      ; _
      }
    =
    fill
  in
  let t =
    apply_one
      t
      ~participant:aggressor_participant
      ~symbol
      ~side:aggressor_side
      ~price
      ~size
  in
  apply_one
    t
    ~participant:resting_participant
    ~symbol
    ~side:(Side.flip aggressor_side)
    ~price
    ~size
;;

let apply_trade_report t (report : Trade_report.t) =
  { t with
    references = Map.set t.references ~key:report.symbol ~data:report.price
  }
;;

module Summary = struct
  type per_symbol =
    { symbol : Symbol_id.t
    ; shares : int
    ; average_entry : float
    ; reference_price : float option
    ; realized : float
    ; unrealized : float
    }
  [@@deriving sexp_of, fields ~getters]

  type t =
    { per_symbol : per_symbol list
    ; total_realized : float
    ; total_unrealized : float
    ; total : float
    }
  [@@deriving sexp_of, fields ~getters]
end

let cents_to_dollars cents = cents /. 100.

let summary t participant =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol_id.Map.empty
  in
  let per_symbol =
    Map.to_alist by_symbol
    |> List.map ~f:(fun (symbol, (position : Position.t)) ->
      let reference_cents =
        Map.find t.references symbol
        |> Option.map ~f:(fun price ->
          Float.of_int (Price.to_int_cents price))
      in
      let unrealized_cents =
        match reference_cents with
        | None -> 0.
        | Some reference_cents ->
          Float.of_int position.shares
          *. (reference_cents -. position.average_entry_cents)
      in
      { Summary.symbol
      ; shares = position.shares
      ; average_entry = cents_to_dollars position.average_entry_cents
      ; reference_price = Option.map reference_cents ~f:cents_to_dollars
      ; realized = cents_to_dollars position.realized_cents
      ; unrealized = cents_to_dollars unrealized_cents
      })
  in
  let total_realized =
    List.sum (module Float) per_symbol ~f:Summary.realized
  in
  let total_unrealized =
    List.sum (module Float) per_symbol ~f:Summary.unrealized
  in
  { Summary.per_symbol
  ; total_realized
  ; total_unrealized
  ; total = total_realized +. total_unrealized
  }
;;
