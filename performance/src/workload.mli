(** Deterministic workload generation for load-testing the matching engine
    (Part 4, Exercise 6).

    A {!t} turns a seed and a {!Config.t} into an endless stream of
    {!Action.t}s — submits and cancels shaped by the config — for a replay
    driver to pump directly into {!Jsip_order_book.Matching_engine} calls. No
    RPC and no Async: the point is to saturate the engine so a profiler sees
    engine work, not scheduler sleep.

    The generator is closed-loop: after executing an action, the driver
    passes the resulting events to {!observe} so the generator's picture of
    the resting book stays in sync with fills. Same seed + same config = the
    same action stream, which is what makes before/after performance
    comparisons valid.

    {[
      let gen = Workload.create Workload.Config.balanced ~seed:42 in
      let events =
        match Workload.next_action gen with
        | Submit { participant; request } ->
          Matching_engine.submit engine ~participant request
        | Cancel { participant; client_order_id } ->
          Matching_engine.cancel engine participant client_order_id
      in
      Workload.observe gen events
    ]} *)

open! Core
open Jsip_types

module Config : sig
  (** The shape of the traffic. Fractions are probabilities in [0, 1];
      "reference price" is the generator's own per-symbol price anchor, which
      random-walks by at most [drift_cents] per action. *)
  type t =
    { num_symbols : int
    (** The engine trades symbol ids [0 .. num_symbols - 1]. *)
    ; num_participants : int
    (** Actions are attributed round-robin-ish (seeded) across this many
        distinct participants. *)
    ; cancel_fraction : float
    (** Probability that the next action cancels a live resting order. When
        nothing is resting, the generator falls back to a submit. *)
    ; marketable_fraction : float
    (** Of submits: the fraction priced to cross the opposite side and trade
        immediately. The rest are placed behind the touch. *)
    ; ioc_fraction : float
    (** Of submits: the fraction sent [Ioc]; the rest are [Day]. A
        non-marketable [Ioc] cancels immediately instead of resting. *)
    ; max_order_size : int
    (** Order sizes are drawn uniformly from [1, max_order_size]. *)
    ; initial_price_cents : int
    (** Every symbol's reference price at the start of the run. *)
    ; drift_cents : int
    (** Per-action cap on the reference price's random walk. [0] pins the
        reference price for the whole run. *)
    ; resting_band_cents : int
    (** Non-marketable orders are placed up to this many cents behind the
        reference price on their own side (bids below it, asks above). *)
    }
  [@@deriving sexp_of]

  (** A steady-state mix of submits and cancels: book depth should plateau,
      the BBO should hover near [initial_price_cents], and a meaningful share
      of submits should fill. *)
  val balanced : t
end

module Action : sig
  (** One engine call for the replay driver to make. The driver must feed the
      events the call returns back into {!observe}. *)
  type t =
    | Submit of
        { participant : Participant.t
        ; request : Order.Request.t
        }
    | Cancel of
        { participant : Participant.t
        ; client_order_id : Client_order_id.t
        }
  [@@deriving sexp_of]
end

type t

(** All randomness flows from [seed] via [Splittable_random], so equal seeds
    and configs yield equal action streams. *)
val create : Config.t -> seed:int -> t

(** The next action to execute. Client order ids are fresh for the lifetime
    of [t] — the engine never forgets a used id. *)
val next_action : t -> Action.t

(** Report the events the engine returned for the last action, so the
    generator can drop filled or cancelled orders from its live resting set. *)
val observe : t -> Exchange_event.t list -> unit
