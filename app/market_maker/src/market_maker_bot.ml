open! Core
open! Async
open Jsip_types
open Jsip_gateway
(* module Fundamental_oracle = Jsip_fundamental.Fundamental_oracle

module Context = struct
  type t =
    { participant : Participant.t
    ; oracle : Fundamental_oracle.t
    ; rng : Splittable_random.t
    ; dispatch_submit : Order.Request.t -> unit Deferred.Or_error.t
    ; dispatch_cancel : Client_order_id.t -> unit Deferred.Or_error.t
    }

  let participant t = t.participant
  let fundamental t symbol = Fundamental_oracle.price t.oracle symbol
  let random t = t.rng
  let submit t request = t.dispatch_submit request
  let cancel t client_order_id = t.dispatch_cancel client_order_id
end *)




