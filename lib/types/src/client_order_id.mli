open! Core

type t [@@deriving sexp, bin_io, compare, equal, hash, string]

module For_testing: sig
    val of_int: int -> t
    val to_int: t -> t
end