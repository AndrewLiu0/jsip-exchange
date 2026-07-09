(** A server-local integer identity for a participant.

    Minted by {!Registry.intern} at login: the first login of a name gets the
    next dense id (0, 1, 2, ...); later logins of the same name (e.g.
    reconnects) get the same id back. Ids are meaningful only within one
    server run and never cross the wire — clients and wire types speak
    {!Participant.t} names, and the server resolves ids back to names at its
    edges via {!Registry.name}.

    Lives in the gateway rather than [lib/types] deliberately: an id is
    server-local state tied to a registry, not wire data. *)

open! Core
open Jsip_types

type t = private int [@@deriving compare, equal, hash, sexp_of]

(** The name <-> id mapping, shared across all connections for the whole
    server run. It is additive: ids are never dropped, so any id ever handed
    out (a long-lived session's, or either party of a fill) keeps resolving
    after that participant disconnects. Contrast the dispatcher's session
    table, which tracks who is connected {e now} and prunes on disconnect —
    different job, different lifetime. *)
module Registry : sig
  type id := t
  type t

  val create : unit -> t

  (** The id already assigned to [participant], or the next dense id if this
      name has never been seen. *)
  val intern : t -> Participant.t -> id

  (** Like {!intern} but never mints: [None] for a name that has never logged
      in (e.g. when routing an event for an unknown participant). *)
  val find : t -> Participant.t -> id option

  (** Resolve an id back to its name, for edges that speak names. Raises on
      an id this registry never minted — unreachable via the public API,
      since ids only come from {!intern}. *)
  val name : t -> id -> Participant.t
end
