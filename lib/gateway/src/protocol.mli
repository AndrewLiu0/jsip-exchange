(** Text protocol for communicating with the exchange.

    This module defines how order requests are represented as text and how
    exchange events are formatted for display. On a production exchange, this
    would be a binary protocol like FIX for performance and interoperability.
    We use a simple human-readable text format for ease of debugging and
    interactive use.

    {2 Command format}

    Each command is a single line of text:
    {v
    BUY  <symbol> <size> <price> [<time_in_force>] [as <participant>]
    SELL <symbol> <size> <price> [<time_in_force>] [as <participant>]
    v}

    Examples:
    {v
    BUY AAPL 100 150.25
    SELL TSLA 50 200.00 IOC
    BUY AAPL 100 150.00 DAY as Alice
    v}

    Time-in-force defaults to DAY if omitted. Participant defaults to
    "anonymous" if omitted. *)

open! Core
open Jsip_types

(** Parse a text command into an order request. Returns [Error] with a
    human-readable message if the input is malformed. *)
val parse_command : string -> (Order.Request.t, string) Result.t

(** Like [parse_command], but uses [default] as the participant if none is
    specified in the command text. Useful for clients that already know their
    identity. *)
val parse_command_with_default_participant
  :  string
  -> default:Participant.t
  -> (Order.Request.t, string) Result.t

(** Format an exchange event as a single line of human-readable text. *)
val format_event : Exchange_event.t -> string

(** Format a list of events, one per line. *)
val format_events : Exchange_event.t list -> string
