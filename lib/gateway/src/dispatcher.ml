open! Core
open! Async
open Jsip_types

type t =
  { market_data_subscribers_by_symbol :
      Exchange_event.t Pipe.Writer.t Bag.t Symbol_id.Table.t
  ; audit_subscribers : Exchange_event.t Pipe.Writer.t Bag.t
  ; registry : Participant_id.Registry.t
  ; directory : Symbol_directory.t
  ; id_to_session : (Participant_id.t, Session.t) Hashtbl.t
  }

let create registry ~directory =
  { market_data_subscribers_by_symbol = Symbol_id.Table.create ()
  ; audit_subscribers = Bag.create ()
  ; registry
  ; directory
  ; id_to_session = Hashtbl.create (module Participant_id)
  }
;;

let subscribe_market_data t symbols =
  let reader, writer = Pipe.create () in
  (* Ids come straight from the client's subscription request, so filter out
     anything the directory doesn't know — this keeps the table's keys valid
     by construction. A subscription to only unknown ids yields a pipe that
     never fires, matching the old behavior for unknown symbol strings. *)
  let symbols =
    List.filter symbols ~f:(fun id ->
      Option.is_some (Symbol_directory.name t.directory id))
  in
  (* Register the same writer in every requested symbol's bag. A per-symbol
     publish iterates a single bag, so a subscriber listed in multiple bags
     receives each event exactly once — only via whichever bag matches the
     event's symbol. *)
  let elts =
    List.map symbols ~f:(fun symbol ->
      let subscribers =
        Hashtbl.find_or_add
          t.market_data_subscribers_by_symbol
          ~default:Bag.create
          symbol
      in
      symbol, Bag.add subscribers writer)
  in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     List.iter elts ~f:(fun (symbol, elt) ->
       match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
       | None -> ()
       | Some subscribers -> Bag.remove subscribers elt));
  reader
;;

let subscribe_audit t =
  let reader, writer = Pipe.create () in
  let elt = Bag.add t.audit_subscribers writer in
  don't_wait_for
    (let%map () = Pipe.closed writer in
     Bag.remove t.audit_subscribers elt);
  reader
;;

let push_market_data t event symbol =
  match Hashtbl.find t.market_data_subscribers_by_symbol symbol with
  | None -> ()
  | Some subscribers ->
    Bag.iter subscribers ~f:(fun writer ->
      Pipe.write_without_pushback_if_open writer event)
;;

let push_audit t event =
  Bag.iter t.audit_subscribers ~f:(fun writer ->
    Pipe.write_without_pushback_if_open writer event)
;;

let clean_up_session t session =
  Hashtbl.remove t.id_to_session (Session.participant_id session);
  return (Session.close session)
;;

(* Helper for exchange_server login *)
let register_session t session =
  let participant_id = Session.participant_id session in
  if Hashtbl.mem t.id_to_session participant_id
  then Or_error.error_string "Participant already logged onto exchange"
  else (
    Hashtbl.set t.id_to_session ~key:participant_id ~data:session;
    Ok ())
;;

(* Events carry participant names (the engine speaks names), so routing
   resolves name -> id -> session. [Registry.find], not [intern]: a
   participant who never logged in (e.g. seeded engine-side) should not be
   assigned an id just because an event mentions them. *)
let push_to_session t participant event =
  let session =
    let open Option.Let_syntax in
    let%bind id = Participant_id.Registry.find t.registry participant in
    Hashtbl.find t.id_to_session id
  in
  match session with
  | Some session -> Session.push session event
  | None ->
    print_endline
      [%string
        "[for %{participant#Participant}] %{Protocol.format_event event}"]
;;

let dispatch_event t (event : Exchange_event.t) =
  push_audit t event;
  match event with
  | Best_bid_offer_update { symbol; bbo = _ } ->
    push_market_data t event symbol
  | Trade_report { symbol; price = _; size = _ } ->
    push_market_data t event symbol
  | Order_accept { order_id = _; participant; request = _ }
  | Order_reject { participant; request = _; reason = _ }
  | Cancel_reject { participant; client_order_id = _; reason = _ } ->
    push_to_session t participant event
  | Order_cancel
      { order_id = _
      ; client_order_id = _
      ; participant
      ; symbol = _
      ; remaining_size = _
      ; reason = _
      } ->
    push_to_session t participant event
  | Fill
      { fill_id = _
      ; symbol = _
      ; price = _
      ; size = _
      ; aggressor_client_order_id = _
      ; aggressor_order_id = _
      ; aggressor_participant
      ; aggressor_side = _
      ; resting_client_order_id = _
      ; resting_order_id = _
      ; resting_participant
      } ->
    push_to_session t aggressor_participant event;
    push_to_session t resting_participant event
;;

let dispatch t events = List.iter events ~f:(dispatch_event t)

(* Queue-length accessors for the stats snapshot. Sorted so snapshots are
   deterministic under expect tests. [Pipe.length] on a writer counts the
   values buffered but not yet read — exactly the "how far behind is this
   subscriber" number. *)

let market_data_queue_lengths t =
  Hashtbl.to_alist t.market_data_subscribers_by_symbol
  |> List.filter_map ~f:(fun (id, subscribers) ->
    (* The snapshot speaks names (like its participant columns), so resolve
       at this edge. [subscribe_market_data] only admits directory-known ids,
       so the filter_map never actually drops anything. *)
    match Symbol_directory.name t.directory id with
    | None -> None
    | Some symbol ->
      Some (symbol, List.map (Bag.to_list subscribers) ~f:Pipe.length))
  |> List.sort ~compare:(fun (s1, _) (s2, _) -> Symbol.compare s1 s2)
;;

let audit_queue_lengths t =
  List.map (Bag.to_list t.audit_subscribers) ~f:Pipe.length
;;

let session_queue_lengths t =
  Hashtbl.data t.id_to_session
  |> List.map ~f:(fun session ->
    Session.participant session, Session.queue_length session)
  |> List.sort ~compare:(fun (p1, _) (p2, _) -> Participant.compare p1 p2)
;;

module For_testing = struct
  let audit_subscriber_count t = Bag.length t.audit_subscribers
end
