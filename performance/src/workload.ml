open! Core
open Jsip_types

module Config = struct
  type t =
    { num_symbols : int
    ; num_participants : int
    ; cancel_fraction : float
    ; marketable_fraction : float
    ; ioc_fraction : float
    ; max_order_size : int
    ; initial_price_cents : int
    ; drift_cents : int
    ; resting_band_cents : int
    }
  [@@deriving sexp_of]

  let balanced =
    { num_symbols = 100
    ; num_participants = 100
    ; cancel_fraction = 0.50
    ; marketable_fraction = 0.50
    ; ioc_fraction = 0.50
    ; max_order_size = 500
    ; initial_price_cents = 15000
    ; drift_cents = 30
    ; resting_band_cents = 1000
    }
  ;;
end

module Action = struct
  type t =
    | Submit of
        { participant : Participant.t
        ; request : Order.Request.t
        }
    | Cancel of
        { participant : Participant.t
        ; client_order_id : Client_order_id.t
        }
  [@@deriving sexp_of]
end

(* A resting order the generator believes is still on the book. [remaining]
   tracks unfilled size so partial fills don't retire the order early. *)
type live_order =
  { participant : Participant.t
  ; client_order_id : Client_order_id.t
  ; mutable remaining : int
  }

(* Client ids come from one shared generator (never per-participant), so an
   id's int is unique across the whole run and can key [live] directly.
   [live_keys] is a companion array for O(1) uniformly-random cancel picks;
   it is cleaned lazily, so it may hold ids of orders that have already left
   the book. *)
type t =
  { config : Config.t
  ; random : Splittable_random.t
  ; participants : Participant.t array
  ; ref_prices : int array (* per symbol id, in cents *)
  ; client_order_ids : Client_order_id.Generator.t
  ; live : live_order Int.Table.t
  ; live_keys : int Dynarray.t
  }

let create (config : Config.t) ~seed =
  { config
  ; random = Splittable_random.of_int seed
  ; participants =
      Array.init config.num_participants ~f:(fun i ->
        Participant.of_string [%string "P%{i#Int}"])
  ; ref_prices =
      Array.create ~len:config.num_symbols config.initial_price_cents
  ; client_order_ids = Client_order_id.Generator.create ()
  ; live = Int.Table.create () (* Ground truth of live orders *)
  ; live_keys =
      Dynarray.create () (* Tombstone, used for picking a random element *)
  }
;;

(* Stale ids accumulate in [live_keys] between lazy deletions; rebuild from
   the table when they dominate, so memory stays proportional to the book. *)
let compact_live_keys_if_needed t =
  let len = Dynarray.length t.live_keys in
  if len > 64 && len > 4 * Hashtbl.length t.live
  then (
    Dynarray.clear t.live_keys;
    Hashtbl.iter_keys t.live ~f:(fun key ->
      Dynarray.add_last t.live_keys key))
;;

(* Uniformly random live order, or [None] if nothing rests. A picked slot
   whose order is gone is swap-removed and the pick retried, so each stale
   entry costs one extra draw, once. *)
let rec pick_live t =
  match Dynarray.length t.live_keys with
  | 0 -> None
  | n ->
    let i = Splittable_random.int t.random ~lo:0 ~hi:(n - 1) in
    let key = Dynarray.get t.live_keys i in
    (match Hashtbl.find t.live key with
     | Some order -> Some order
     | None ->
       (* Clearing tombstones *)
       let last = Dynarray.pop_last t.live_keys in
       if i < Dynarray.length t.live_keys
       then Dynarray.set t.live_keys i last;
       pick_live t)
;;

(* Marketable orders are priced a full band through the reference, so they
   reach everything resting in the opposite zone even after the reference has
   drifted some since those orders were placed. Resting orders land uniformly
   across their own side's band ([1 .. band] cents behind the reference),
   giving a wide multi-level book rather than one deep queue. The clamp
   matters on marketable sells once the reference has walked near the
   one-cent floor. *)
let price_cents t ~reference ~side ~marketable =
  let band = t.config.resting_band_cents in
  let behind = Splittable_random.int t.random ~lo:1 ~hi:band in
  let price =
    match (side : Side.t) with
    | Buy -> if marketable then reference + band else reference - behind
    | Sell -> if marketable then reference - band else reference + behind
  in
  Int.max 1 price
;;

let generate_submit t =
  let config = t.config in
  let symbol_index =
    Splittable_random.int t.random ~lo:0 ~hi:(config.num_symbols - 1)
  in
  (* Drift the reference price before pricing off it, clamped away from zero
     so [Price.of_int_cents] stays valid. *)
  if config.drift_cents > 0
  then (
    let delta =
      Splittable_random.int
        t.random
        ~lo:(-config.drift_cents)
        ~hi:config.drift_cents
    in
    t.ref_prices.(symbol_index)
    <- Int.max 1 (t.ref_prices.(symbol_index) + delta));
  let reference = t.ref_prices.(symbol_index) in
  let side : Side.t =
    if Splittable_random.bool t.random then Buy else Sell
  in
  let marketable =
    Float.( < )
      (Splittable_random.float t.random ~lo:0. ~hi:1.)
      config.marketable_fraction
  in
  let time_in_force : Time_in_force.t =
    if Float.( < )
         (Splittable_random.float t.random ~lo:0. ~hi:1.)
         config.ioc_fraction
    then Ioc
    else Day
  in
  let participant =
    t.participants.(Splittable_random.int
                      t.random
                      ~lo:0
                      ~hi:(config.num_participants - 1))
  in
  Action.Submit
    { participant
    ; request =
        { client_order_id =
            Client_order_id.Generator.generate t.client_order_ids
        ; symbol = Symbol_id.of_int_exn symbol_index
        ; side
        ; price =
            Price.of_int_cents (price_cents t ~reference ~side ~marketable)
        ; size =
            Size.of_int
              (Splittable_random.int
                 t.random
                 ~lo:1
                 ~hi:config.max_order_size)
        ; time_in_force
        }
    }
;;

let next_action t =
  compact_live_keys_if_needed t;
  let want_cancel =
    Float.( < )
      (Splittable_random.float t.random ~lo:0. ~hi:1.)
      t.config.cancel_fraction
  in
  match if want_cancel then pick_live t else None with
  | Some { participant; client_order_id; remaining = _ } ->
    Action.Cancel { participant; client_order_id }
  | None -> generate_submit t
;;

let decrement_live t client_order_id ~by =
  let key = Client_order_id.to_int client_order_id in
  match Hashtbl.find t.live key with
  | None -> ()
  | Some order ->
    order.remaining <- order.remaining - by;
    if order.remaining <= 0 then Hashtbl.remove t.live key
;;

(* Within one [submit]'s events the accept precedes its fills, so a Day
   aggressor is added at full size here and immediately decremented by the
   fills that follow — leaving exactly its resting remainder. *)
let observe t events =
  List.iter events ~f:(fun (event : Exchange_event.t) ->
    match event with
    | Order_accept { order_id = _; participant; request } ->
      (match request.time_in_force with
       | Ioc -> ()
       | Day ->
         let key = Client_order_id.to_int request.client_order_id in
         Hashtbl.set
           t.live
           ~key
           ~data:
             { participant
             ; client_order_id = request.client_order_id
             ; remaining = Size.to_int request.size
             };
         Dynarray.add_last t.live_keys key)
    | Fill fill ->
      decrement_live
        t
        fill.resting_client_order_id
        ~by:(Size.to_int fill.size);
      decrement_live
        t
        fill.aggressor_client_order_id
        ~by:(Size.to_int fill.size)
    | Order_cancel { client_order_id; _ } ->
      Hashtbl.remove t.live (Client_order_id.to_int client_order_id)
    | Order_reject _ | Cancel_reject _ -> ()
    | Best_bid_offer_update _ | Trade_report _ -> ())
;;
