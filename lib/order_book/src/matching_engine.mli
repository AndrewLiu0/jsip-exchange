(** The matching engine: receives order requests, manages order books, and
    produces exchange events.

    The engine is the heart of the exchange. It assigns order IDs, determines
    which orders can trade against each other, executes fills, and manages
    the lifecycle of resting orders. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** Create a matching engine trading [num_symbols] symbols: one order book
    per id in [0 .. num_symbols - 1]. The engine never sees symbol names —
    ids are assigned by the server's symbol directory (id = position in the
    startup symbol list), and every request arrives carrying the id. *)
val create : num_symbols:int -> t

(** {2 Order submission} *)

(** Submit a new order request on behalf of [participant]. Returns the list
    of exchange events produced: an acceptance or rejection, followed by any
    fills, and possibly a cancellation of unfilled remainder (for IOC
    orders).

    A request is rejected (with a single [Order_reject] event) if:

    - the [(participant, client_order_id)] pair has already been used by a
      prior accepted submission — even if that order is now fully filled or
      cancelled. IDs are never reused within the lifetime of the engine.
    - the request's [symbol] id is not one this engine trades. The id comes
      straight off the wire, so this covers out-of-range and negative ids
      from buggy or hostile clients, not just unknown symbols.

    The event list is always non-empty (at minimum an acceptance or
    rejection). *)
val submit
  :  t
  -> participant:Participant.t
  -> Order.Request.t
  -> Exchange_event.t list

(** Cancel a resting order submitted by the participant under the given
    client order ID.

    On success, returns the events to publish — at minimum an [Order_cancel]
    with [Participant_requested] reason, plus a [Best_bid_offer_update] if
    the cancellation changed the BBO.

    Returns [Cancel_reject] with an "order not found" reason when the
    participant never submitted an order under this ID, or when the order is
    no longer in the book (fully filled, or previously cancelled) — the
    client cannot distinguish the two cases. *)
val cancel : t -> Participant.t -> Client_order_id.t -> Exchange_event.t list

(** {2 Queries} *)

(** The order book for a given symbol id, or [None] if the id is not one this
    engine trades (including out-of-range wire values). *)
val book : t -> Symbol_id.t -> Order_book.t option
