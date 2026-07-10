(** Tests for the market maker, using a real exchange server. *)

open! Core
open! Async
open Jsip_types
open Jsip_test_harness
open Jsip_market_maker
open E2e_helpers

let default_config : Market_maker.Config.t =
  { participant = Harness.market_maker
  ; symbol = Harness.aapl_id
  ; fair_value_cents = 15000
  ; half_spread_cents = 10
  ; size_per_level = 100
  ; num_levels = 3
  }
;;

let%expect_test "seed_book: places symmetric bids and asks around fair value"
  =
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let ids = Client_order_id.Generator.create () in
    let%bind () =
      Market_maker.seed_book default_config (connection mm) ~ids
    in
    [%expect
      {|
      [for MarketMaker] ACCEPTED id=1 0 BUY 100@$149.90 DAY
      [for MarketMaker] ACCEPTED id=2 0 SELL 100@$150.10 DAY
      [for MarketMaker] ACCEPTED id=3 0 BUY 100@$149.89 DAY
      [for MarketMaker] ACCEPTED id=4 0 SELL 100@$150.11 DAY
      [for MarketMaker] ACCEPTED id=5 0 BUY 100@$149.88 DAY
      [for MarketMaker] ACCEPTED id=6 0 SELL 100@$150.12 DAY
      |}];
    return ())
;;

let%expect_test "seed_book: reseeding with the shared generator issues \
                 fresh ids, so nothing is rejected as a duplicate"
  =
  (* Regression test: seed_book used to create a new generator per call,
     restarting ids at 1. The exchange never forgets a used client_order_id,
     so every submission after the first call was rejected — the
     -trade-back-and-forth demo silently stopped trading after its first
     cycle. With the caller-owned generator the second seeding must continue
     at id=7 and be accepted. *)
  with_server ~symbols:[ Harness.aapl ] (fun ~server:_ ~port ->
    let%bind mm = connect_as ~port Harness.market_maker in
    let ids = Client_order_id.Generator.create () in
    let%bind () =
      Market_maker.seed_book default_config (connection mm) ~ids
    in
    let%bind () =
      Market_maker.seed_book default_config (connection mm) ~ids
    in
    [%expect
      {|
      [for MarketMaker] ACCEPTED id=1 0 BUY 100@$149.90 DAY
      [for MarketMaker] ACCEPTED id=2 0 SELL 100@$150.10 DAY
      [for MarketMaker] ACCEPTED id=3 0 BUY 100@$149.89 DAY
      [for MarketMaker] ACCEPTED id=4 0 SELL 100@$150.11 DAY
      [for MarketMaker] ACCEPTED id=5 0 BUY 100@$149.88 DAY
      [for MarketMaker] ACCEPTED id=6 0 SELL 100@$150.12 DAY
      [for MarketMaker] ACCEPTED id=7 0 BUY 100@$149.90 DAY
      [for MarketMaker] ACCEPTED id=8 0 SELL 100@$150.10 DAY
      [for MarketMaker] ACCEPTED id=9 0 BUY 100@$149.89 DAY
      [for MarketMaker] ACCEPTED id=10 0 SELL 100@$150.11 DAY
      [for MarketMaker] ACCEPTED id=11 0 BUY 100@$149.88 DAY
      [for MarketMaker] ACCEPTED id=12 0 SELL 100@$150.12 DAY
      |}];
    return ())
;;
