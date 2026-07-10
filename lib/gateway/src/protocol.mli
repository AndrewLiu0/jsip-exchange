(** Text protocol for communicating with the exchange.

    This module defines how order requests are represented as text and how
    exchange events are formatted for display. On a production exchange, this
    would be a binary protocol like FIX for performance and interoperability.
    We use a simple human-readable text format for ease of debugging and
    interactive use.

    {2 Command format}

    Each command is a single line of text:
    {v
    BUY  <client_order_id> <symbol-id> <size> <price> [<time_in_force>]
    SELL <client_order_id> <symbol-id> <size> <price> [<time_in_force>]
    v}

    Examples:
    {v
    BUY 1 0 100 150.25
    SELL 2 1 50 200.00 IOC
    v}

    Time-in-force defaults to DAY if omitted. The submitting participant is
    not part of the command — identity is established at connection time via
    {!Rpc_protocol.login_rpc} and attached by the server. (The interactive
    client's richer parser, {!Exchange_command}, accepts symbol {e names} and
    resolves them through the directory; this legacy parser speaks raw ids.)

    {2 Rendering}

    The formatters below are where consumers recover human-readable symbol
    names: each takes a [lookup] resolving a wire {!Symbol_id.t} to its name,
    typically [Symbol_directory.name mirror] over the directory fetched at
    connect. Ids the lookup doesn't know render as ["#<id>"]. Pass
    [~lookup:(fun _ -> None)] to render raw ids everywhere (the Phase-1
    behavior). The [to_string] functions in [lib/types] stay pure and print
    raw ids; these formatters are deliberately the only place names re-enter. *)

open! Core
open Jsip_types

(** Parse a text command into an order request. Returns [Error] with a
    human-readable message if the input is malformed. *)
val parse_command : string -> (Order.Request.t, string) Result.t

(** Resolve one id for display: the name if [lookup] knows it, ["#<id>"]
    otherwise. The building block the formatters below share; exposed for
    consumers rendering a bare id (e.g. the monitor's BBO panel). *)
val render_symbol
  :  lookup:(Symbol_id.t -> Symbol.t option)
  -> Symbol_id.t
  -> string

(** Format an exchange event as a single line of human-readable text. *)
val format_event
  :  lookup:(Symbol_id.t -> Symbol.t option)
  -> Exchange_event.t
  -> string

(** Format a list of events, one per line. *)
val format_events
  :  lookup:(Symbol_id.t -> Symbol.t option)
  -> Exchange_event.t list
  -> string

(** As {!Fill.to_participant_view}, with the symbol resolved to a name:
    ["You sold 100 AAPL at $150.25"]. *)
val fill_participant_view
  :  lookup:(Symbol_id.t -> Symbol.t option)
  -> Fill.t
  -> Participant.t
  -> string option

(** As {!Book.to_string}, with the header symbol resolved to a name. *)
val format_book : lookup:(Symbol_id.t -> Symbol.t option) -> Book.t -> string
