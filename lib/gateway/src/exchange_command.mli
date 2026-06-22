open! Core
open Jsip_types


module Verb: sig
  type t = Buy | Sell | Book | Subscribe
end
[@@deriving string ~case_insensitive ~to_string ~of_string]

type t =
  | Submit of Order.Request.t
  | Book of Symbol.t
  | Subscribe of Symbol.t

[@@deriving
  sexp
  , bin_io
  , compare
  , equal
  , enumerate
  , hash
  , string]



val parse : ?default_participant:Participant.t -> string -> t Or_error.t
