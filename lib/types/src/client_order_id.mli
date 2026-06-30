open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]


module Generator : sig
  type order_id := t
  type t [@@deriving sexp_of]

  val create : unit -> t
  val next : t -> order_id
end


module For_testing: sig
    val of_int: int -> t
    val to_int: t -> t
end