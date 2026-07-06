open! Core
open Jsip_types

let%expect_test "notional_cents: price * size" =
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15025
     ; size = Size.of_int 100
     ; aggressor_client_order_id = Client_order_id.For_testing.of_int 1
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Buy
     ; resting_client_order_id = Client_order_id.For_testing.of_int 1
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = Participant.of_string "Bob"
     }
     : Fill.t)
  in
  [%test_result: int] (Fill.notional_cents fill) ~expect:1502500
;;

let%expect_test "participant view" =
  let bob = Participant.of_string "Bob" in
  let fill =
    ({ fill_id = 1
     ; symbol = Symbol.of_string "AAPL"
     ; price = Price.of_int_cents 15025
     ; size = Size.of_int 100
     ; aggressor_client_order_id = Client_order_id.For_testing.of_int 1
     ; aggressor_order_id = Order_id.of_string "1"
     ; aggressor_participant = Participant.of_string "Alice"
     ; aggressor_side = Buy
     ; resting_client_order_id = Client_order_id.For_testing.of_int 1
     ; resting_order_id = Order_id.of_string "2"
     ; resting_participant = bob
     }
     : Fill.t)
  in
  print_s [%sexp (Fill.to_participant_view fill bob : string option)];
  [%expect {| ("You sold 100 AAPL at $150.25") |}]
;;
