open! Core

module T = struct
  type t = int [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let of_int_exn id =
  if id < 0
  then
    raise_s
      [%message "Symbol_id.of_int_exn: id must be non-negative" (id : int)];
  id
;;

(* Shadows the derived [of_string] so text input (e.g. a command typed at the
   client) gets the same non-negativity check as [of_int_exn]. The derived
   [t_of_sexp]/[bin_read_t] still bypass it: machine formats are validated
   where the id is used, not here. *)
let of_string s = of_int_exn (Int.of_string s)
let to_int t = t
