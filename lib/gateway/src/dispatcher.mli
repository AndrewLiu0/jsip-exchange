(** Central event-routing component for the gateway.

    Owns subscription registries:

    - **Market-data subscribers**, keyed by [Symbol.t]. Each subscriber gets
      a pipe of [Best_bid_offer_update] and [Trade_report] events for the
      symbol they asked about. This is the public market-data feed.

    - **Audit subscribers**, an unfiltered firehose of every event the
      matching engine produces. Intended for the exchange operator's monitor;
      not appropriate to expose to ordinary clients.

    [dispatch] is the single place that decides "for each event, who gets
    it". *)

open! Core
open! Async
open Jsip_types

type t

(** Create a dispatcher.

    The registry is shared with the rest of the server: session routing is
    keyed by {!Participant_id.t}, so dispatching an event resolves the
    participant name it carries to an id first. Events for a participant with
    no live session fall back to stdout (the server binary prints them; tests
    silence them). *)
val create : Participant_id.Registry.t -> t

(** Subscribe to public market data for one or more [symbols]. The same pipe
    receives events for every requested symbol; the dispatcher avoids
    duplicates so a subscriber listed against multiple symbols only sees each
    event once. The pipe is removed from the dispatcher when its reader is
    closed. *)
val subscribe_market_data
  :  t
  -> Symbol.t list
  -> Exchange_event.t Pipe.Reader.t

(** Subscribe to the full unfiltered event firehose. Intended for the monitor
    / admin tools. *)
val subscribe_audit : t -> Exchange_event.t Pipe.Reader.t

(** Route each event to every interested subscriber:

    - Every event is pushed to every audit subscriber.
    - [Best_bid_offer_update] and [Trade_report] are pushed to the
      market-data subscribers that asked for the event's symbol.
    - [Order_accept], [Order_cancel], and [Order_reject] are pushed to the
      session of the order's owning participant (if logged in).
    - [Fill] is pushed to both the aggressor's and the resting party's
      session (if either is logged in).

    Each session lookup is O(1) and independent of subscriber count. *)
val dispatch : t -> Exchange_event.t list -> unit

val clean_up_session : t -> Session.t -> unit Deferred.t
val register_session : t -> Session.t -> unit Or_error.t

(** {2 Queue-length accessors}

    Inputs to the pipe-occupancy pane of the stats stream (see
    {!Jsip_stats.Snapshot.Pipe_occupancy}). Each reports, per subscriber
    pipe, how many events are buffered awaiting the consumer — the number
    that grows without bound when a subscriber stops reading, because
    dispatch writes without pushback. Results are sorted by symbol /
    participant so snapshots are deterministic. *)

(** Per symbol, one length per market-data subscriber pipe. *)
val market_data_queue_lengths : t -> (Symbol.t * int list) list

(** One length per audit-log subscriber pipe. *)
val audit_queue_lengths : t -> int list

(** One length per logged-in session's outbound feed. *)
val session_queue_lengths : t -> (Participant.t * int) list

module For_testing : sig
  val audit_subscriber_count : t -> int
end
