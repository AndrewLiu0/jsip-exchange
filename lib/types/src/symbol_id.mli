(** A dense integer identifier for a trading symbol.

    Ids are assigned at server startup: the symbol at position [i] of the
    server's symbol list gets id [i], so an id doubles as an index into
    per-symbol arrays (e.g. the matching engine's order books). Wire types
    ({!Order.Request}, {!Book}, {!Fill}, {!Exchange_event}) carry the id
    instead of the {!Symbol.t} name: an int is cheaper to ship on every
    message and cheaper to resolve than a string. Consumers that need the
    human name recover it through the symbol directory served by the exchange
    ([Symbol_directory] in the gateway); [to_string] here prints just the
    int, e.g. ["7"].

    Contrast with [Participant_id], which is deliberately server-local (no
    [bin_io], lives in the gateway): a [Symbol_id.t] {e does} cross the wire,
    so it lives here with the other wire types and derives [bin_io].

    Because of that, an id arriving off the wire cannot be trusted — [bin_io]
    deserialization can produce {e any} int, negative included — so every
    id-indexed lookup must bounds-check rather than index blindly (see
    [Matching_engine] in [lib/order_book]). *)

open! Core

type t = private int [@@deriving sexp, bin_io, compare, equal, hash, string]

include Comparable.S with type t := t
include Hashable.S with type t := t

(** The id numbered [i]. Raises if [i] is negative. Only positions in the
    server's symbol list are meaningful values; anything else will be
    rejected wherever the id is looked up. *)
val of_int_exn : int -> t

val to_int : t -> int
