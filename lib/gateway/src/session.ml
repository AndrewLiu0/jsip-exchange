open! Core
open! Async
open Jsip_types

type t =
  { participant : Participant.t
  ; participant_id : Participant_id.t
  ; reader : Exchange_event.t Pipe.Reader.t
  ; writer : Exchange_event.t Pipe.Writer.t
  }

let create participant ~participant_id =
  let reader, writer = Pipe.create () in
  { participant; participant_id; reader; writer }
;;

let participant t = t.participant
let participant_id t = t.participant_id
let reader t = t.reader
let queue_length t = Pipe.length t.writer
let push t event = Pipe.write_without_pushback_if_open t.writer event
let close t = Pipe.close t.writer
let is_closed t = Pipe.is_closed t.writer
