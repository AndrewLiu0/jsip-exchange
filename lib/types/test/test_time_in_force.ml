open! Core
open Jsip_types

let%test_unit "rests_on_book: Day rests, Ioc does not" =
  [%test_result: bool] (Time_in_force.rests_on_book Day) ~expect:true;
  [%test_result: bool] (Time_in_force.rests_on_book Ioc) ~expect:false
;;
