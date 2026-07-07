open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let of_int t = t
let to_int t = t

module Generator = struct
  type t = { mutable next_id : int } [@@deriving sexp_of]

  let create () = { next_id = 1 }

  let generate t =
    let id = of_int t.next_id in
    t.next_id <- t.next_id + 1;
    id
  ;;
end

module For_testing = struct
  let of_int = of_int
  let to_int = to_int
end
