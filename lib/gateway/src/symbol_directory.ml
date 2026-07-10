open! Core
open Jsip_types

type t =
  { ids : Symbol_id.t Symbol.Map.t
  ; names : Symbol.t Symbol_id.Map.t
  }
[@@deriving sexp_of]

let of_alist_exn alist =
  let ids = Symbol.Map.of_alist_exn alist in
  let names =
    Symbol_id.Map.of_alist_exn
      (List.map alist ~f:(fun (symbol, id) -> id, symbol))
  in
  (* Dense ids are what let the matching engine index its book array by id;
     reject anything else before it can cause confusion downstream. All ids
     are distinct (the map build raises on duplicates), so "every id in
     [0, num_symbols)" is equivalent to dense. The negative check matters:
     bin_io can deserialize a negative int into a Symbol_id.t. *)
  Map.iteri names ~f:(fun ~key:id ~data:(_ : Symbol.t) ->
    if Symbol_id.to_int id < 0 || Symbol_id.to_int id >= Map.length names
    then
      raise_s
        [%message
          "Symbol_directory.of_alist_exn: ids must be dense from 0"
            (id : Symbol_id.t)
            ~num_symbols:(Map.length names : int)]);
  { ids; names }
;;

let of_symbols symbols =
  of_alist_exn
    (List.mapi symbols ~f:(fun id symbol -> symbol, Symbol_id.of_int_exn id))
;;

let id t symbol = Map.find t.ids symbol
let name t id = Map.find t.names id

let to_alist t =
  Map.to_alist t.names |> List.map ~f:(fun (id, symbol) -> symbol, id)
;;

let num_symbols t = Map.length t.names
