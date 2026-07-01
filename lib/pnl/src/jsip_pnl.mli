(** Per-participant profit-and-loss tracking for the JSIP exchange.

    Folds the exchange's fill and trade-print stream into realized and
    unrealized P&L, broken down per participant and symbol. *)

module Pnl = Pnl
