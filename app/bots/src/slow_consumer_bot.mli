open! Core
open! Async
open Jsip_types
module Context = Jsip_bot_runtime.Bot_runtime.Context

(* Logs in and reads its subscriber feed very slowly (or not at all),
   stalling the read side of its pipe. Because the exchange writes to each
   subscriber without pushback, the exchange-side buffer holding events for
   this subscriber grows without bound.

   The bot has no way to choose its own subscriptions -- the runner always
   subscribes it to the session feed and, when the scenario sets
   [is_marketdata_consumer], to market data. Since this bot never submits,
   its session feed is nearly empty, so the pressure comes from market data:
   the driving scenario must run it as a market-data consumer. *)

(** The bot does not operate on any symbols, since it is only reading; not
    submitting requests. *)
module Config : sig
  type t =
    { read_delay : Time_ns.Span.t
    (** How long [on_event] waits in between reading/returning each event.
        Stalls the read side of its subscriber pipe, forcing the
        exchange-side buffer to grow. Set this to be very large to simulate a
        never-reading client. *)
    ; events_per_read : int
    (** Number of events the bot reads before sleeping again. *)
    ; events_seen : int ref
    (** Mutable state, to keep track of the number of events read so that we
        know when to go back to sleep. *)
    }
  [@@deriving sexp_of]
end

val name : string
val on_start : Config.t -> Context.t -> unit Deferred.t
val on_tick : Config.t -> Context.t -> unit Deferred.t
val on_event : Config.t -> Context.t -> Exchange_event.t -> unit Deferred.t
