(** The symbol name <-> id mapping.

    The authoritative directory is built once, in the server binary's [main],
    from the ordered symbol list the exchange trades: {!of_symbols} gives the
    symbol at position [i] the id [i], matching the matching engine's book
    array (which is indexed by {!Symbol_id.t}). Consumers (client, monitor,
    bots) fetch the [(name, id)] pairs over the symbol-directory RPC and
    rebuild a mirror with {!of_alist_exn}.

    Ids are always dense [0 .. num_symbols - 1]; both constructors enforce
    this, so a [t] can never disagree with the engine about which ids exist.
    The set is immutable — the exchange's symbol universe is fixed for the
    server's lifetime — so unlike [Participant_id.Registry] (which interns
    participants dynamically at login) there is no [intern]: the whole
    mapping exists from the moment the directory is built.

    Lookups return [option] in both directions because both inputs can be
    untrusted: a name typed by a human may not be traded, and an id off the
    wire may be out of range. Nothing here is on a hot path — resolution
    happens at parse, render, and snapshot time, not per message. *)

open! Core
open Jsip_types

type t [@@deriving sexp_of]

(** The directory giving the symbol at position [i] the id [i]. Raises on
    duplicate symbols. *)
val of_symbols : Symbol.t list -> t

(** Rebuild a directory from the pairs served by the symbol-directory RPC.
    Raises on duplicate names and on ids that are not exactly
    [0 .. length - 1]. *)
val of_alist_exn : (Symbol.t * Symbol_id.t) list -> t

(** The id [symbol] trades under, or [None] if it is not traded. *)
val id : t -> Symbol.t -> Symbol_id.t option

(** The name behind [id], or [None] for an id this directory never issued
    (e.g. an out-of-range value off the wire). *)
val name : t -> Symbol_id.t -> Symbol.t option

(** All [(name, id)] pairs in id order — the payload the directory RPC
    serves. *)
val to_alist : t -> (Symbol.t * Symbol_id.t) list

val num_symbols : t -> int
