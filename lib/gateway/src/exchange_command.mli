open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t
[@@deriving sexp]

(** Parse one line of user input. [lookup] resolves a symbol name to its wire
    id — typically [Symbol_directory.id mirror] over the directory the client
    fetched at connect — so "unknown symbol" surfaces {e here}, at parse
    time, rather than round-tripping to the server as it did when the wire
    carried names. (The server still validates the id: a hand-rolled client
    can send anything.) Symbol names are matched case-insensitively by
    uppercasing the input, mirroring how BUY/SELL/DAY keywords are parsed. *)
val parse : lookup:(Symbol.t -> Symbol_id.t option) -> string -> t Or_error.t
