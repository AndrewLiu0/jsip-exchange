open! Core
open Jsip_types
open Jsip_order_book
open Jsip_gateway

let print_parse line =
  match Protocol.parse_command line with
  | Error msg -> print_endline [%string "ERROR: %{msg}"]
  | Ok req -> print_endline [%string "%{req#Order.Request}"]
;;

(* --- Successful parsing --- *)

let%expect_test "parse: basic buy" =
  print_parse "BUY AAPL 100 150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL TSLA 50 200.00";
  [%expect {| SELL TSLA 50@$200.00 DAY as anonymous |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy AAPL 100 150.00";
  print_parse "Buy AAPL 100 150.00";
  [%expect
    {|
    BUY AAPL 100@$150.00 DAY as anonymous
    BUY AAPL 100@$150.00 DAY as anonymous
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY AAPL 100 150.00 IOC";
  [%expect {| BUY AAPL 100@$150.00 IOC as anonymous |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL AAPL 200 151.00 DAY";
  [%expect {| SELL AAPL 200@$151.00 DAY as anonymous |}]
;;

let%expect_test "parse: with participant" =
  print_parse "BUY AAPL 100 150.00 as Alice";
  [%expect {| BUY AAPL 100@$150.00 DAY as Alice |}]
;;

let%expect_test "parse: with TIF and participant" =
  print_parse "SELL GOOG 75 2800.50 IOC as Bob";
  [%expect {| SELL GOOG 75@$2800.50 IOC as Bob |}]
;;

let%expect_test "parse: symbol is uppercased" =
  print_parse "BUY aapl 100 150.00";
  [%expect {| BUY aapl 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse "  BUY   AAPL   100   150.00  ";
  [%expect {| BUY AAPL 100@$150.00 DAY as anonymous |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY AAPL 100 $150.25";
  [%expect {| BUY AAPL 100@$150.25 DAY as anonymous |}]
;;

(* --- Parse errors --- *)

let%expect_test "parse error: empty string" =
  print_parse "";
  print_parse "   ";
  [%expect {|
    ERROR: empty command
    ERROR: empty command
    |}]
;;

let%expect_test "parse error: unknown command" =
  print_parse "HOLD AAPL 100 150.00";
  [%expect {| ERROR: unknown command: HOLD (expected BUY or SELL) |}]
;;

let%expect_test "parse error: missing fields" =
  print_parse "BUY AAPL";
  print_parse "BUY";
  [%expect
    {|
    ERROR: expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]
    ERROR: expected: BUY|SELL <symbol> <size> <price> [DAY|IOC] [as <name>]
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY AAPL abc 150.00";
  print_parse "BUY AAPL 0 150.00";
  print_parse "BUY AAPL -5 150.00";
  [%expect
    {|
    ERROR: invalid size: abc
    ERROR: size must be positive
    ERROR: size must be positive
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY AAPL 100 xyz";
  [%expect
    {|
    ERROR: invalid price: xyz
    exception: (Invalid_argument "Float.of_string xyz")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY AAPL 100 150.00 QQQ";
  [%expect {| ERROR: unknown time-in-force: QQQ (expected DAY or IOC) |}]
;;

(* --- parse_command_with_default_participant --- *)

let%expect_test "default participant: used when none specified" =
  let default = Participant.of_string "DefaultTrader" in
  let req =
    match
      Exchange_command.parse
        ~default_participant:default
        "BUY AAPL 100 150.00"
      |> ok_exn
    with
    | Submit r -> r
    | Book _ | Subscribe _ -> failwith "Expected order"
  in
  print_endline [%string "participant=%{req.participant#Participant}"];
  [%expect {| participant=DefaultTrader |}]
;;

let%expect_test "default participant: overridden by explicit 'as'" =
  let default = Participant.of_string "DefaultTrader" in
  let req =
    match
      Exchange_command.parse
        ~default_participant:default
        "BUY AAPL 100 150.00 as Alice"
      |> ok_exn
    with
    | Submit r -> r
    | Book _ | Subscribe _ -> failwith "Expected order"
  in
  print_endline [%string "participant=%{req.participant#Participant}"];
  [%expect {| participant=Alice |}]
;;

(* 8c Additional Tests *)

let%expect_test "BOOK with a symbol argument" =
  let default = Participant.of_string "DefaultTrader" in
  let symbol =
    match
      Exchange_command.parse ~default_participant:default "BOOK AAPL"
      |> ok_exn
    with
    | Book s -> s
    | Submit _ | Subscribe _ -> failwith "Expected order"
  in
  print_endline [%string "symbol=%{symbol#Symbol}"];
  [%expect {| symbol=AAPL |}]
;;

let%expect_test "SUBSCRIBE with case-insensitive input" =
  let default = Participant.of_string "DefaultTrader" in
  let symbol =
    match
      Exchange_command.parse ~default_participant:default "subsCribe AAPL"
      |> ok_exn
    with
    | Subscribe s -> s
    | Submit _ |  Book _ -> failwith "Expected order"
  in
  print_endline [%string "symbol=%{symbol#Symbol}"];
  [%expect {| symbol=AAPL |}]
;;

(* --- Event formatting --- *)

let%expect_test "format_event: all event types" =
  let events =
    [ Exchange_event.Order_accept
        { order_id = Order_id.of_string "1"
        ; request =
            { symbol = Symbol.of_string "AAPL"
            ; participant = Participant.of_string "Alice"
            ; side = Buy
            ; price = Price.of_int_cents 15000
            ; size = Size.of_int 100
            ; time_in_force = Day
            }
        }
    ; Fill
        { fill_id = 1
        ; symbol = Symbol.of_string "AAPL"
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        ; aggressor_order_id = Order_id.of_string "2"
        ; aggressor_participant = Participant.of_string "Alice"
        ; aggressor_side = Buy
        ; resting_order_id = Order_id.of_string "1"
        ; resting_participant = Participant.of_string "Bob"
        }
    ; Order_cancel
        { order_id = Order_id.of_string "3"
        ; participant = Participant.of_string "Charlie"
        ; symbol = Symbol.of_string "TSLA"
        ; remaining_size = Size.of_int 50
        ; reason = Ioc_remainder
        }
    ; Order_reject
        { request =
            { symbol = Symbol.of_string "GOOG"
            ; participant = Participant.of_string "Alice"
            ; side = Sell
            ; price = Price.of_int_cents 28000
            ; size = Size.of_int 10
            ; time_in_force = Day
            }
        ; reason = "unknown symbol"
        }
    ; Best_bid_offer_update
        { symbol = Symbol.of_string "AAPL"
        ; bbo =
            { bid =
                Some
                  { price = Price.of_int_cents 14990
                  ; size = Size.of_int 200
                  }
            ; ask =
                Some
                  { price = Price.of_int_cents 15010
                  ; size = Size.of_int 100
                  }
            }
        }
    ; Best_bid_offer_update
        { symbol = Symbol.of_string "AAPL"; bbo = Bbo.empty }
    ; Trade_report
        { symbol = Symbol.of_string "AAPL"
        ; price = Price.of_int_cents 15000
        ; size = Size.of_int 100
        }
    ]
  in
  List.iter events ~f:(fun e -> print_endline (Protocol.format_event e));
  [%expect
    {|
    ACCEPTED id=1 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    CANCELLED id=3 TSLA remaining=50 reason=IOC_REMAINDER
    REJECTED GOOG SELL 10@$280.00 reason=unknown symbol
    BBO AAPL bid=$149.90 x200 ask=$150.10 x100
    BBO AAPL bid=- ask=-
    TRADE AAPL $150.00 x100
    |}]
;;

(* --- Round-trip: parse then format --- *)

let%expect_test "round-trip: parse a command, submit, format result" =
  let open Jsip_test_harness in
  let t = Harness.create () in
  (* Place a resting sell *)
  Harness.submit_
    t
    (Harness.sell ~price_cents:15000 ~participant:Harness.bob ());
  (* Parse a buy command from text and submit it *)
  let request =
    Protocol.parse_command "BUY AAPL 100 150.00 as Alice"
    |> Result.map_error ~f:Error.of_string
    |> ok_exn
  in
  let events = Matching_engine.submit (Harness.engine t) request in
  print_endline (Protocol.format_events events);
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    BBO AAPL bid=- ask=$150.00 x100
    ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2(Alice) BUY resting=1(Bob)
    TRADE AAPL $150.00 x100
    BBO AAPL bid=- ask=-
    |}]
;;
