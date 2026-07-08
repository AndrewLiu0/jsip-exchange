open! Core

module List_seq = struct
  (* A [ref] so [set] can mutate in place (same trick as [Silly_store]'s list
     ref): an immutable list can only be "updated" by rebuilding it. *)
  type t = int list ref

  let create () = ref []

  let set t ~key ~data =
    let length = List.length !t in
    if key < 0 || key > length
    then
      raise_s
        [%message
          "List_seq.set: index out of range" (key : int) (length : int)]
    else if key = length
    then t := List.append !t [ data ]
    else t := List.mapi !t ~f:(fun i x -> if i = key then data else x)
  ;;

  let get t key = List.nth !t key
end

module Dynarray_seq = struct
  (* Stdlib [Dynarray] (OCaml >= 5.2); not shadowed by [Core]. *)
  type t = int Dynarray.t

  let create () = Dynarray.create ()

  let set t ~key ~data =
    let length = Dynarray.length t in
    if key < 0 || key > length
    then
      raise_s
        [%message
          "Dynarray_seq.set: index out of range" (key : int) (length : int)]
    else if key = length
    then Dynarray.add_last t data
    else Dynarray.set t key data
  ;;

  (* Stdlib [Dynarray.get] raises out of range; our interface wants [None]. *)
  let get t key =
    if key < 0 || key >= Dynarray.length t
    then None
    else Some (Dynarray.get t key)
  ;;
end
