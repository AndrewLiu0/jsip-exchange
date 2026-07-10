open! Core
open! Async
open Jsip_types
open Jsip_bot_runtime.Bot_runtime

module State = struct
  type t =
    { mutable inventory : int
    (** Net position in [symbol]: filled buys add, filled sells subtract. *)
    ; outstanding : Size.t Hashtbl.M(Client_order_id).t
    (** Remaining size of each order we believe is resting on the book. *)
    ; ids : Client_order_id.Generator.t
    }
  [@@deriving sexp_of]

  let create () =
    { inventory = 0
    ; outstanding = Hashtbl.create (module Client_order_id)
    ; ids = Client_order_id.Generator.create ()
    }
  ;;
end

module Config = struct
  type t =
    { symbol : Symbol.t
    ; half_spread_cents : int
    ; size_per_level : int
    ; num_levels : int
    ; inventory_skew_cents_per_share : int
    ; state : State.t
    }
  [@@deriving sexp_of]

  let create
    ~symbol
    ~half_spread_cents
    ~size_per_level
    ~num_levels
    ~inventory_skew_cents_per_share
    =
    { symbol
    ; half_spread_cents
    ; size_per_level
    ; num_levels
    ; inventory_skew_cents_per_share
    ; state = State.create ()
    }
  ;;
end

let name = "market_maker"

(* When the market maker is long, the skewed fair value drops below the
   fundamental, pulling both sides of the ladder down: buyers are less
   tempted by the cheaper bid and sellers are more tempted by the cheaper
   ask, so subsequent fills push inventory back toward zero. *)
let skewed_fair_cents (config : Config.t) ctx =
  let fundamental_cents =
    Price.to_int_cents (Context.fundamental ctx config.symbol)
  in
  fundamental_cents
  - (config.state.inventory * config.inventory_skew_cents_per_share)
;;

(* Given a fill and this bot's participant, return the signed change to
   inventory: positive if this fill increased our position (we bought),
   negative if it decreased it (we sold), and [0] if we were not involved in
   the fill at all.

   Remember that a fill has two parties, and we could be either one:
   [fill.aggressor_participant] (our incoming order caused the match) or
   [fill.resting_participant] (someone traded against our resting order).
   [fill.aggressor_side] is the side of the *aggressor* — if the aggressor
   bought, the resting party sold, and vice versa. Self-trade prevention
   guarantees we are never both parties. Sizes are [Size.to_int fill.size]
   shares. *)
let inventory_delta (fill : Fill.t) ~me : int =
  match
    ( Participant.equal me fill.aggressor_participant
    , Participant.equal me fill.resting_participant )
  with
  | true, _ -> Side.sign fill.aggressor_side * Size.to_int fill.size
  | _, true ->
    Side.sign (Side.flip fill.aggressor_side) * Size.to_int fill.size
  | false, false -> 0
;;

let post_ladder (config : Config.t) ctx =
  let fair_cents = skewed_fair_cents config ctx in
  let submit (side : Side.t) ~offset =
    let price_cents =
      match side with
      | Buy -> fair_cents - offset
      | Sell -> fair_cents + offset
    in
    let request : Order.Request.t =
      { client_order_id = Client_order_id.Generator.generate config.state.ids
      ; symbol = Context.symbol_id_exn ctx config.symbol
      ; side
      ; price = Price.of_int_cents price_cents
      ; size = Size.of_int config.size_per_level
      ; time_in_force = Day
      }
    in
    match%map Context.submit ctx request with
    | Ok () -> ()
    | Error err ->
      [%log.error
        "market_maker: submit failed"
          (request : Order.Request.t)
          (err : Error.t)]
  in
  Deferred.List.iter
    ~how:`Sequential
    (List.init config.num_levels ~f:Fn.id)
    ~f:(fun level ->
      let offset = config.half_spread_cents + level in
      let%bind () = submit Buy ~offset in
      submit Sell ~offset)
;;

(* Cancel everything we believe is resting. Cancels raced by fills come back
   as errors ("order not found"); per the exercise we ignore them — the order
   is gone either way. Ids are sorted so tests see a stable order. *)
let cancel_all_outstanding (config : Config.t) ctx =
  let ids =
    Hashtbl.keys config.state.outstanding
    |> List.sort ~compare:Client_order_id.compare
  in
  Hashtbl.clear config.state.outstanding;
  Deferred.List.iter ~how:`Sequential ids ~f:(fun id ->
    match%map Context.cancel ctx id with
    | Ok () -> ()
    | Error (_ : Error.t) -> ())
;;

let apply_fill (config : Config.t) ctx (fill : Fill.t) =
  let state = config.state in
  let me = Context.participant ctx in
  match inventory_delta fill ~me with
  | 0 -> return ()
  | delta ->
    state.inventory <- state.inventory + delta;
    let my_client_order_id =
      match Participant.equal fill.aggressor_participant me with
      | true -> fill.aggressor_client_order_id
      | false -> fill.resting_client_order_id
    in
    Hashtbl.change state.outstanding my_client_order_id ~f:(function
      | None -> None
      | Some remaining ->
        let remaining = Size.to_int remaining - Size.to_int fill.size in
        (match remaining > 0 with
         | true -> Some (Size.of_int remaining)
         | false -> None));
    let%bind () = cancel_all_outstanding config ctx in
    post_ladder config ctx
;;

let on_start (config : Config.t) ctx = post_ladder config ctx

(* Quotes only move in response to fills. Re-quoting off the drifting
   fundamental on a slow clock is a natural extension here. *)
let on_tick (_ : Config.t) _ctx = return ()

let on_event (config : Config.t) ctx (event : Exchange_event.t) =
  let state = config.state in
  let me = Context.participant ctx in
  match event with
  | Fill fill -> apply_fill config ctx fill
  | Order_accept { participant; request; order_id = _ } ->
    (match Participant.equal participant me with
     | true ->
       Hashtbl.set
         state.outstanding
         ~key:request.client_order_id
         ~data:request.size
     | false -> ());
    return ()
  | Order_cancel { client_order_id; participant; _ } ->
    (match Participant.equal participant me with
     | true -> Hashtbl.remove state.outstanding client_order_id
     | false -> ());
    return ()
  | Order_reject _ | Cancel_reject _ | Best_bid_offer_update _
  | Trade_report _ ->
    return ()
;;

module For_testing = struct
  let inventory (config : Config.t) = config.state.inventory

  let outstanding (config : Config.t) =
    Hashtbl.keys config.state.outstanding
    |> List.sort ~compare:Client_order_id.compare
  ;;

  let inventory_delta = inventory_delta
end
