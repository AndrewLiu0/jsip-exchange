open! Core
open Jsip_types

let%test_unit "flip: Buy <-> Sell" =
  [%test_result: Side.t] (Side.flip Buy) ~expect:Sell;
  [%test_result: Side.t] (Side.flip Sell) ~expect:Buy
;;

let%test_unit "sign: Buy = 1, Sell = -1" =
  [%test_result: int] (Side.sign Buy) ~expect:1;
  [%test_result: int] (Side.sign Sell) ~expect:(-1)
;;
