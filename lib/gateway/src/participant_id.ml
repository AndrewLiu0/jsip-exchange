open! Core
open Jsip_types

type t = int [@@deriving compare, equal, hash, sexp_of]

module Registry = struct
  type nonrec t =
    { ids : t Participant.Table.t (** name -> id *)
    ; names : Participant.t Dynarray.t (** id = index; login order *)
    }

  let create () =
    { ids = Participant.Table.create (); names = Dynarray.create () }
  ;;

  let intern t participant =
    Hashtbl.find_or_add t.ids participant ~default:(fun () ->
      Dynarray.add_last t.names participant;
      Dynarray.length t.names - 1)
  ;;

  let find t participant = Hashtbl.find t.ids participant
  let name t id = Dynarray.get t.names id
end
