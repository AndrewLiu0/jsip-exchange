(** Text protocol for communicating with the exchange.

    This module defines how order requests are represented as text and how
    exchange events are formatted for display. On a production exchange, this
    would be a binary protocol like FIX for performance and interoperability.
    We use a simple human-readable text format for ease of debugging and
    interactive use.

    {2 Command format}

    Each command is a single line of text:
    {v
    BUY  <client_order_id> <symbol> <size> <price> [<time_in_force>]
    SELL <client_order_id> <symbol> <size> <price> [<time_in_force>]
    v}

    Examples:
    {v
    BUY 1 AAPL 100 150.25
    SELL 2 TSLA 50 200.00 IOC
    v}

    Time-in-force defaults to DAY if omitted. The submitting participant is
    not part of the command — identity is established at connection time via
    {!Rpc_protocol.login_rpc} and attached by the server. *)

open! Core
open Jsip_types

(** Parse a text command into an order request. Returns [Error] with a
    human-readable message if the input is malformed. *)
val parse_command : string -> (Order.Request.t, string) Result.t

(** Format an exchange event as a single line of human-readable text. *)
val format_event : Exchange_event.t -> string

(** Format a list of events, one per line. *)
val format_events : Exchange_event.t list -> string
