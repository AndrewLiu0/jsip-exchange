open! Core
open Jsip_types

module Trade_report = struct
  type t =
    { symbol : Symbol.t
    ; price : Price.t
    ; size : Size.t
    }
  [@@deriving sexp_of]

  let create ~symbol ~price ~size = { symbol; price; size }

  let of_exchange_event : Exchange_event.t -> t option = function
    | Trade_report { symbol; price; size } -> Some { symbol; price; size }
    | Order_accept _
    | Fill _
    | Order_cancel _
    | Order_reject _
    | Best_bid_offer_update _
    | Cancel_reject _ -> None
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
     flat) blends [price] into [average_entry_cents], weighted by the
     shares on each side. The blended average is what later reductions are
     measured against.
   - Reducing or closing realizes cash on the closed shares:
     [closed * (price - average_entry_cents)] for a long, and the mirror
     image for a short. If the trade crosses through zero, the leftover
     shares open a fresh position at [price], and the average entry resets
     to [price]. When the position returns exactly to flat, the average
     entry is meaningless — reset it to [0.].

   Work in cents throughout; convert to dollars only at the {!Summary}
   boundary. *)
let apply_execution
  (position : Position.t)
  ~(side : Side.t)
  ~(price : Price.t)
  ~(size : Size.t)
  : Position.t
  =
  let { Position.shares = old_shares
      ; average_entry_cents = old_avg
      ; realized_cents = realized
      }
    =
    position
  in
  let qty = Size.to_int size in
  let price_cents = Float.of_int (Price.to_int_cents price) in
  let signed_qty = Side.sign side * qty in
  let same_direction =
    (old_shares > 0 && signed_qty > 0) || (old_shares < 0 && signed_qty < 0)
  in
  if old_shares = 0 || same_direction
  then (
    (* Opening or adding: blend [price] into the average, weighted by the
       shares resting on each side. *)
    let new_shares = old_shares + signed_qty in
    let old_cost = Float.of_int (abs old_shares) *. old_avg in
    let added_cost = Float.of_int qty *. price_cents in
    let average_entry_cents =
      (old_cost +. added_cost) /. Float.of_int (abs new_shares)
    in
    { Position.shares = new_shares; average_entry_cents; realized_cents = realized })
  else (
    (* Reducing, closing, or flipping: realize P&L on the shares that close
       against the existing position. [position_sign] makes shorts profit
       when they buy back below their average. *)
    let closed = Int.min (abs old_shares) qty in
    let position_sign = if old_shares > 0 then 1 else -1 in
    let realized_cents =
      realized
      +. (Float.of_int (closed * position_sign) *. (price_cents -. old_avg))
    in
    let new_shares = old_shares + signed_qty in
    let average_entry_cents =
      if new_shares = 0
      then 0. (* back to flat: no open cost basis *)
      else if abs old_shares < qty
      then price_cents (* crossed through zero: reopened at the trade price *)
      else old_avg (* partial reduction: the average is unchanged *)
    in
    { Position.shares = new_shares; average_entry_cents; realized_cents })
;;

type t =
  { positions : Position.t Symbol.Map.t Participant.Map.t
  ; references : Price.t Symbol.Map.t
  }
[@@deriving sexp_of]

let empty =
  { positions = Participant.Map.empty; references = Symbol.Map.empty }
;;

let position t participant symbol =
  Map.find t.positions participant
  |> Option.bind ~f:(fun by_symbol -> Map.find by_symbol symbol)
  |> Option.value ~default:Position.flat
;;

let set_position t participant symbol position =
  let by_symbol =
    Map.find t.positions participant
    |> Option.value ~default:Symbol.Map.empty
  in
  let by_symbol = Map.set by_symbol ~key:symbol ~data:position in
  { t with
    positions = Map.set t.positions ~key:participant ~data:by_symbol
  }
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
    references =
      Map.set t.references ~key:report.symbol ~data:report.price
  }
;;

module Summary = struct
  type per_symbol =
    { symbol : Symbol.t
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
    |> Option.value ~default:Symbol.Map.empty
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
