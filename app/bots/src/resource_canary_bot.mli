open! Core
open! Async
open Jsip_bot_runtime
open Jsip_types

(** Description **)

(** This bot's purpose is to measure the effect of misbehaving bots in the
    ecosystem on the latency of resource requests to the server. On every
    tick that is a multiple of request interval, it requests a book query
    from the server for the specified symbols and records the latency of the
    requests. On every multiple of report interval, it prints collected
    statistics to the terminal **)

module Per_symbol_data : sig
  (** Per-symbol latency bookkeeping: a bounded window of the most recent
      latencies plus a running sum and sample count for the overall average.
      The representation is abstract so its invariants stay internal to the
      bot. *)
  type t
end

module RCConfig : sig
  type t =
    { participant : Participant.t (* participant name *)
    ; request_interval : int (* how many clock ticks between requests? *)
    ; report_interval : int (* how many clock ticks between reports? *)
    ; symbols : Symbol.t list (* Which books are we tracking latency for? *)
    ; book_query :
        Symbol.t
        -> Book.t option Deferred.t (* function to call the book_query_rpc *)
    ; latency_data : Per_symbol_data.t Symbol.Table.t
        (* per-symbol latency bookkeeping, keyed by symbol *)
    ; mutable ticks_since_start : int (* ticks since the bot was started *)
    }
end

include Bot_runtime.Bot with type Config.t = RCConfig.t

module For_testing : sig
  (** Build a [Per_symbol_data.t] directly from its components. [recent] is
      the bounded window of recent latencies (oldest first); [sum_latencies]
      and [num_samples] cover every sample ever seen. *)
  val create_data
    :  recent:float list
    -> sum_latencies:float
    -> num_samples:int
    -> Per_symbol_data.t

  val num_samples : Per_symbol_data.t -> int
end
