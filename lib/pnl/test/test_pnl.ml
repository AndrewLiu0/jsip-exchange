open! Core
open Jsip_types
open Jsip_pnl
open Jsip_test_harness

(* Build a [Fill.t] with the ids we don't care about zeroed out. A fill is
   symmetric: [aggressor] trades [aggressor_side], [resting] the flip. *)
let fill ~aggressor ~aggressor_side ~resting ~symbol ~price_cents ~size =
  { Fill.fill_id = 0
  ; symbol
  ; price = Price.of_int_cents price_cents
  ; size = Size.of_int size
  ; aggressor_client_order_id = Client_order_id.For_testing.of_int 0
  ; aggressor_order_id = Order_id.For_testing.of_int 0
  ; aggressor_participant = aggressor
  ; aggressor_side
  ; resting_client_order_id = Client_order_id.For_testing.of_int 0
  ; resting_order_id = Order_id.For_testing.of_int 0
  ; resting_participant = resting
  }
;;

let print_summary label t participant =
  let summary : Pnl.Summary.t = Pnl.summary t participant in
  printf "%s\n" label;
  List.iter summary.per_symbol ~f:(fun row ->
    printf
      "  %s: shares=%d avg=$%.2f ref=%s realized=$%.2f unrealized=$%.2f\n"
      (Symbol_id.to_string row.symbol)
      row.shares
      row.average_entry
      (Option.value_map
         row.reference_price
         ~default:"-"
         ~f:(sprintf "$%.2f"))
      row.realized
      row.unrealized);
  printf
    "  TOTAL: realized=$%.2f unrealized=$%.2f pnl=$%.2f\n"
    summary.total_realized
    summary.total_unrealized
    summary.total
;;

let%expect_test "build, blend, partially close, then mark" =
  (* Alice buys 100 @ $150, then 100 more @ $160 (avg entry $155), sells 40 @
     $170 (realizing $600 on the closed shares), and the market prints at
     $180 to mark the remaining 160 shares. The market maker is the resting
     counterparty on every fill, so it carries the mirror-image short. *)
  let pnl =
    List.fold
      ~init:Pnl.empty
      ~f:Pnl.apply_fill
      [ fill
          ~aggressor:Harness.alice
          ~aggressor_side:Buy
          ~resting:Harness.market_maker
          ~symbol:Harness.aapl_id
          ~price_cents:15000
          ~size:100
      ; fill
          ~aggressor:Harness.alice
          ~aggressor_side:Buy
          ~resting:Harness.market_maker
          ~symbol:Harness.aapl_id
          ~price_cents:16000
          ~size:100
      ; fill
          ~aggressor:Harness.alice
          ~aggressor_side:Sell
          ~resting:Harness.market_maker
          ~symbol:Harness.aapl_id
          ~price_cents:17000
          ~size:40
      ]
  in
  let pnl =
    Pnl.apply_trade_report
      pnl
      (Pnl.Trade_report.create
         ~symbol:Harness.aapl_id
         ~price:(Price.of_int_cents 18000)
         ~size:(Size.of_int 10))
  in
  print_summary "alice" pnl Harness.alice;
  print_summary "market_maker" pnl Harness.market_maker;
  [%expect
    {|
    alice
      0: shares=160 avg=$155.00 ref=$180.00 realized=$600.00 unrealized=$4000.00
      TOTAL: realized=$600.00 unrealized=$4000.00 pnl=$4600.00
    market_maker
      0: shares=-160 avg=$155.00 ref=$180.00 realized=$-600.00 unrealized=$-4000.00
      TOTAL: realized=$-600.00 unrealized=$-4000.00 pnl=$-4600.00
    |}]
;;
