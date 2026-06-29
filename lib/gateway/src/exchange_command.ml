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
  | Book of Symbol.t
  | Subscribe of Symbol.t
[@@deriving sexp]

let parse ?default_participant line : t Or_error.t =
  let default_participant =
    match default_participant with
    | None -> Participant.of_string "anonymous"
    | Some s -> s
  in
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
            Or_error.try_with (fun () -> Symbol.of_string symbol_str)
            |> Or_error.tag
                 ~tag:"invalid symbol: %{symbol_str}\nexception: %{exn_str}"
          in
          let%bind time_in_force, rest' =
            match rest with
            | tif_str :: rest' ->
              (match String.uppercase tif_str with
               | "AS" -> Ok (Time_in_force.Day, rest)
               | other ->
                 (try Ok (Time_in_force.of_string other, rest') with
                  | exn ->
                    let exn_str = Exn.to_string exn in
                    Or_error.error_string
                      [%string
                        "Invalid tif str: should be one of %{enumerate_tif}\n\
                        \ exception: %{exn_str}"]))
            | [] -> Ok (Time_in_force.Day, [])
          in
          let%bind participant =
            match rest' with
            | "as" :: name :: _ | "AS" :: name :: _ ->
              Ok (Participant.of_string name)
            | [] -> Ok default_participant
            | _ ->
              let trailing = String.concat ~sep:" " rest in
              Or_error.error_string
                [%string "unexpected trailing arguments: %{trailing}"]
          in
          let order : Order.Request.t =
            { symbol
            ; participant
            ; side = Side.of_string side_str
            ; price
            ; size = Size.of_int size
            ; time_in_force
            ; client_order_id = client_id
            }
          in
          Ok (Submit order)
        | _ ->
          Or_error.error_string
            [%string
              "expected: BUY|SELL <client_order_id> <symbol> <size> <price> \
               [%{enumerate_tif}] [as <name>]"])
     | Book ->
       (match rest with
        | symbol_str :: [] -> Ok (Book (Symbol.of_string symbol_str) : t)
        | _ -> Or_error.error_string "expected a symbol")
     | Subscribe ->
       (match rest with
        | symbol_str :: [] ->
          Ok (Subscribe (Symbol.of_string symbol_str) : t)
        | _ -> Or_error.error_string "expected a symbol"))
;;
