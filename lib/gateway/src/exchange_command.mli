open! Core
open Jsip_types

type t =
  | Submit of Order.Request.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t
[@@deriving sexp]

val parse : string -> t Or_error.t
