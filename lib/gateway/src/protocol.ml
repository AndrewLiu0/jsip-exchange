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

let format_event = function
  | Exchange_event.Order_accept { order_id; participant = _; request } ->
    sprintf
      "ACCEPTED id=%s %s %s %d@%s %s"
      (Order_id.to_string order_id)
      (Symbol_id.to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      (Time_in_force.to_string request.time_in_force)
  | Fill fill -> [%string "FILL %{fill#Fill}"]
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
      (Symbol_id.to_string symbol)
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
      (Symbol_id.to_string request.symbol)
      (Side.to_string request.side)
      (Size.to_int request.size)
      (Price.to_string_dollar request.price)
      reason
  | Best_bid_offer_update { symbol; bbo } ->
    let bid = Level.opt_to_string bbo.bid in
    let ask = Level.opt_to_string bbo.ask in
    [%string "BBO %{symbol#Symbol_id} bid=%{bid} ask=%{ask}"]
  | Trade_report { symbol; price; size } ->
    let size = Size.to_int size in
    [%string "TRADE %{symbol#Symbol_id} %{price#Price} x%{size#Int}"]
;;

let format_events events =
  List.map events ~f:format_event |> String.concat ~sep:"\n"
;;
