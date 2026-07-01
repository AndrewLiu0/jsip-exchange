(** Per-participant, per-symbol profit-and-loss tracking.

    A {!t} accumulates positions as executions stream in. For each
    participant and symbol it tracks:

    - {b inventory} — signed share count ([+] long, [-] short);
    - {b average entry price} — the running cost basis of the open
      position, blended across the trades that built it;
    - {b realized cash} — profit or loss locked in when a position is
      reduced or closed.

    P&L splits into two halves:

    - {b realized} is cash from closed positions — it never changes once
      booked;
    - {b unrealized} is a mark-to-market estimate on the {e open} position,
      [shares * (reference_price - average_entry)], where the reference
      price is the last public trade print (see {!apply_trade_report}).

    Typical use: fold the exchange event stream into a {!t}, feeding
    {!Fill.t} events to {!apply_fill} and trade prints to
    {!apply_trade_report}, then call {!summary} to render a participant's
    book.

    {[
      let pnl =
        List.fold fills ~init:Pnl.empty ~f:Pnl.apply_fill
        |> fun pnl -> Pnl.apply_trade_report pnl last_print
      in
      let summary = Pnl.summary pnl alice
    ]} *)

open! Core
open Jsip_types

(** A public trade print, used only to refresh the reference (mark) price
    for unrealized P&L. This mirrors the [Trade_report] payload of
    {!Exchange_event.t}, which is where these come from on the wire. *)
module Trade_report : sig
  type t =
    { symbol : Symbol.t
    ; price : Price.t
    ; size : Size.t
    }
  [@@deriving sexp_of]

  val create : symbol:Symbol.t -> price:Price.t -> size:Size.t -> t

  (** Extract a trade print from an exchange event, or [None] if the event
      is not a [Trade_report]. Lets you drive {!apply_trade_report} straight
      off the event stream. *)
  val of_exchange_event : Exchange_event.t -> t option
end

(** A participant's open position in a single symbol. *)
module Position : sig
  type t =
    { shares : int (** Signed inventory: [+] long, [-] short, [0] flat. *)
    ; average_entry_cents : float
    (** Average entry price of the open position, in cents. [0.] when
        flat. *)
    ; realized_cents : float (** Running realized P&L, in cents. *)
    }
  [@@deriving sexp_of, fields ~getters]

  (** The zero position: flat, no cost basis, no realized P&L. *)
  val flat : t
end

(** A rendered snapshot of one participant's P&L, in dollars. *)
module Summary : sig
  type per_symbol =
    { symbol : Symbol.t
    ; shares : int
    ; average_entry : float (** Dollars per share. *)
    ; reference_price : float option
    (** Last trade print in dollars, or [None] if none seen for this
        symbol — in which case [unrealized] is [0.]. *)
    ; realized : float (** Dollars. *)
    ; unrealized : float (** Dollars. *)
    }
  [@@deriving sexp_of, fields ~getters]

  type t =
    { per_symbol : per_symbol list
    ; total_realized : float
    ; total_unrealized : float
    ; total : float (** [total_realized +. total_unrealized]. *)
    }
  [@@deriving sexp_of, fields ~getters]
end

type t [@@deriving sexp_of]

(** A tracker with no positions and no reference prices. *)
val empty : t

(** Book a fill against both of its participants: the aggressor trades on
    [aggressor_side], the resting counterparty on the flipped side. *)
val apply_fill : t -> Fill.t -> t

(** Refresh the reference price used for unrealized P&L on the report's
    symbol. Does not touch positions or realized cash. *)
val apply_trade_report : t -> Trade_report.t -> t

(** The current open position for a participant and symbol, or
    {!Position.flat} if none has been established. *)
val position : t -> Participant.t -> Symbol.t -> Position.t

(** Render a participant's per-symbol breakdown and totals. Symbols with no
    activity for this participant are omitted. *)
val summary : t -> Participant.t -> Summary.t
