open! Core
open Jsip_types

module Verb = struct
  type t =
    | Buy
    | Sell
    | Book
    | Subscribe
  [@@deriving string ~case_insensitive]
end

type t =
  | Submit of Order.Request.t
  | Book of Symbol_id.t
  | Subscribe of Symbol_id.t
[@@deriving sexp]

let parse ~lookup line : t Or_error.t =
  let parts = String.split line ~on:' ' in
  match parts with
  | [] -> Or_error.error_string "empty command"
  | side_str :: rest ->
    let open Result.Let_syntax in
    let verb =
      Verb.of_string (String.lowercase side_str |> String.capitalize)
    in
    (match (verb : Verb.t) with
     | Buy | Sell ->
       let tif_to_string =
         List.map ~f:Time_in_force.to_string Time_in_force.all
       in
       let enumerate_tif = String.concat ~sep:"|" tif_to_string in
       (match rest with
        | client_id_str :: symbol_str :: size_str :: price_str :: rest ->
          let%bind client_id =
            Or_error.try_with (fun () ->
              Client_order_id.of_string client_id_str)
            |> Or_error.tag ~tag:[%string "Invalid client_id"]
          in
          let%bind size =
            match Int.of_string_opt size_str with
            | Some n when n > 0 -> Ok n
            | Some _ -> Or_error.error_string "size must be positive"
            | None ->
              Or_error.error_string [%string "invalid size: %{size_str}"]
          in
          let%bind price =
            Or_error.try_with (fun () -> Price.of_string price_str)
            |> Or_error.tag ~tag:[%string "invalid price: %{price_str}"]
          in
          let%bind symbol =
            (* Name -> id through the directory mirror: this is the moment
               the human-typed symbol leaves the world of names. Uppercased
               first so [buy 7 aapl ...] works, matching the BOOK and
               SUBSCRIBE arms. *)
            match
              lookup (Symbol.of_string (String.uppercase symbol_str))
            with
            | Some symbol -> Ok symbol
            | None ->
              Or_error.error_string [%string "unknown symbol: %{symbol_str}"]
          in
          let%bind time_in_force, rest' =
            match rest with
            | tif_str :: rest' ->
              (try Ok (Time_in_force.of_string tif_str, rest') with
               | exn ->
                 let exn_str = Exn.to_string exn in
                 Or_error.error_string
                   [%string
                     "Invalid tif str: should be one of %{enumerate_tif}\n\
                     \ exception: %{exn_str}"])
            | [] -> Ok (Time_in_force.Day, [])
          in
          let%bind () =
            match rest' with
            | [] -> Ok ()
            | _ ->
              let trailing = String.concat ~sep:" " rest' in
              Or_error.error_string
                [%string "unexpected trailing arguments: %{trailing}"]
          in
          let order : Order.Request.t =
            { client_order_id = client_id
            ; symbol
            ; side = Side.of_string side_str
            ; price
            ; size = Size.of_int size
            ; time_in_force
            }
          in
          Ok (Submit order)
        | _ ->
          Or_error.error_string
            [%string
              "expected: BUY|SELL <client_order_id> <symbol> <size> <price> \
               [%{enumerate_tif}]"])
     | Book ->
       (match rest with
        | symbol_str :: [] ->
          (match lookup (Symbol.of_string (String.uppercase symbol_str)) with
           | Some symbol -> Ok (Book symbol : t)
           | None ->
             Or_error.error_string [%string "unknown symbol: %{symbol_str}"])
        | _ -> Or_error.error_string "expected a symbol")
     | Subscribe ->
       (match rest with
        | symbol_str :: [] ->
          (match lookup (Symbol.of_string (String.uppercase symbol_str)) with
           | Some symbol -> Ok (Subscribe symbol : t)
           | None ->
             Or_error.error_string [%string "unknown symbol: %{symbol_str}"])
        | _ -> Or_error.error_string "expected a symbol"))
;;
