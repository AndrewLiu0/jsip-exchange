open! Core
open Jsip_types

let parse_command line =
  let line = String.strip line in
  if String.is_empty line
  then Error "empty command"
  else (
    let parts =
      String.split line ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
    in
    match parts with
    | [] -> Error "empty command"
    | side_str :: rest ->
      let open Result.Let_syntax in
      let%bind side =
        match String.uppercase side_str with
        | "BUY" -> Ok Side.Buy
        | "SELL" -> Ok Side.Sell
        | other ->
          Error [%string "unknown command: %{other} (expected BUY or SELL)"]
      in
      (match rest with
       | client_id_str :: symbol_str :: size_str :: price_str :: rest ->
         let%bind client_id =
           Or_error.try_with (fun () ->
             Client_order_id.of_string client_id_str)
           |> Result.map_error ~f:Error.to_string_hum
         in
         let%bind size =
           match Int.of_string_opt size_str with
           | Some n when n > 0 -> Ok n
           | Some _ -> Error "size must be positive"
           | None -> Error [%string "invalid size: %{size_str}"]
         in
         let%bind price =
           try Ok (Price.of_string price_str) with
           | exn ->
             let exn_str = Exn.to_string exn in
             Error
               [%string "invalid price: %{price_str}\nexception: %{exn_str}"]
         in
         let%bind symbol =
           try Ok (Symbol_id.of_string symbol_str) with
           | exn ->
             let exn_str = Exn.to_string exn in
             Error
               [%string
                 "invalid symbol id: %{symbol_str}\nexception: %{exn_str}"]
         in
         let%bind time_in_force, rest =
           match rest with
           | tif_str :: rest' ->
             (match String.uppercase tif_str with
              | "IOC" -> Ok (Time_in_force.Ioc, rest')
              | "DAY" -> Ok (Day, rest')
              | _ ->
                Error
                  [%string
                    "unknown time-in-force: %{tif_str} (expected DAY or IOC)"])
           | [] -> Ok (Day, [])
         in
         let%bind () =
           match rest with
           | [] -> Ok ()
           | _ ->
             let trailing = String.concat ~sep:" " rest in
             Error [%string "unexpected trailing arguments: %{trailing}"]
         in
         Ok
           ({ client_order_id = client_id
            ; symbol
            ; side
            ; price
            ; size = Size.of_int size
            ; time_in_force
            }
            : Order.Request.t)
       | _ ->
         Error
           "expected: BUY|SELL <client_order_id> <symbol> <size> <price> \
            [DAY|IOC]"))
;;

(* Name recovery lives here, on the consumer side of the wire: the
   [lib/types] renderers stay pure and print raw ids, while these formatters
   resolve ids through the directory mirror the consumer fetched at connect
   ([lookup] is typically [Symbol_directory.name mirror]). An id the mirror
   doesn't know renders as ["#<id>"] rather than masquerading as a name. *)
let render_symbol ~lookup id =
  match (lookup id : Symbol.t option) with
  | Some symbol -> Symbol.to_string symbol
  | None -> [%string "#%{id#Symbol_id}"]
;;

(* Mirrors {!Fill.to_string} (which prints the raw id), with the symbol
   resolved to a name. *)
let format_fill ~lookup (fill : Fill.t) =
  sprintf
    "fill_id=%d %s %s x%d aggressor=%s|%s(%s) %s resting=%s|%s(%s)"
    fill.fill_id
    (render_symbol ~lookup fill.symbol)
    (Price.to_string_dollar fill.price)
    (Size.to_int fill.size)
    (Client_order_id.to_string fill.aggressor_client_order_id)
    (Order_id.to_string fill.aggressor_order_id)
    (Participant.to_string fill.aggressor_participant)
    (Side.to_string fill.aggressor_side)
    (Client_order_id.to_string fill.resting_client_order_id)
    (Order_id.to_string fill.resting_order_id)
    (Participant.to_string fill.resting_participant)
;;

(* Mirrors {!Fill.to_participant_view}, resolving the symbol. *)
let fill_participant_view ~lookup (fill : Fill.t) participant =
  let resting_side = Side.flip fill.aggressor_side in
  let resting_verb =
    match resting_side with Buy -> "bought" | Sell -> "sold"
  in
  if Participant.equal fill.resting_participant participant
  then
    Some
      (sprintf
         "You %s %d %s at %s"
         resting_verb
         (Size.to_int fill.size)
         (render_symbol ~lookup fill.symbol)
         (Price.to_string_dollar fill.price))
  else None
;;

(* Mirrors {!Book.to_string}, resolving the header symbol. *)
let format_book ~lookup ({ symbol; bids; asks; bbo } : Book.t) =
  let format_side label levels =
    match levels with
    | [] -> [%string "  %{label}: (empty)"]
    | _ ->
      let lines =
        List.map levels ~f:(fun level -> [%string "    %{level#Level}"])
        |> String.concat ~sep:"\n"
      in
      [%string "  %{label}:\n%{lines}"]
  in
  let symbol = render_symbol ~lookup symbol in
  String.concat
    ~sep:"\n"
    [ [%string "=== %{symbol} ==="]
    ; format_side "BIDS" bids
    ; format_side "ASKS" asks
    ; [%string "  BBO: %{bbo#Bbo}"]
    ]
;;

let format_event ~lookup = function
  | Exchange_event.Order_accept { order_id; participant = _; request } ->
    sprintf
      "ACCEPTED id=%s %s %s %d@%s %s"
      (Order_id.to_string order_id)
      (render_symbol ~lookup request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      (Time_in_force.to_string request.time_in_force)
  | Fill fill -> [%string "FILL %{format_fill ~lookup fill}"]
  | Order_cancel
      { order_id
      ; client_order_id = _
      ; participant = _
      ; symbol
      ; remaining_size
      ; reason
      } ->
    sprintf
      "CANCELLED id=%s %s remaining=%d reason=%s"
      (Order_id.to_string order_id)
      (render_symbol ~lookup symbol)
      (Size.to_int remaining_size)
      (Cancel_reason.to_string reason)
  | Cancel_reject { participant; client_order_id; reason } ->
    sprintf
      "REJECT CANCELLED client_id = %s for participant %s reason = %s "
      (Client_order_id.to_string client_order_id)
      (Participant.to_string participant)
      reason
  | Order_reject { participant = _; request; reason } ->
    sprintf
      "REJECTED %s %s %d@%s reason=%s"
      (render_symbol ~lookup request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      reason
  | Best_bid_offer_update { symbol; bbo } ->
    let bid = Level.opt_to_string bbo.bid in
    let ask = Level.opt_to_string bbo.ask in
    let symbol = render_symbol ~lookup symbol in
    [%string "BBO %{symbol} bid=%{bid} ask=%{ask}"]
  | Trade_report { symbol; price; size } ->
    let size = Size.to_int size in
    let symbol = render_symbol ~lookup symbol in
    [%string "TRADE %{symbol} %{price#Price} x%{size#Int}"]
;;

let format_events ~lookup events =
  List.map events ~f:(format_event ~lookup) |> String.concat ~sep:"\n"
;;
