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
  print_parse "BUY 1 AAPL 100 150.25";
  [%expect {| BUY 1 AAPL 100@$150.25 DAY |}]
;;

let%expect_test "parse: basic sell" =
  print_parse "SELL 1 TSLA 50 200.00";
  [%expect {| SELL 1 TSLA 50@$200.00 DAY |}]
;;

let%expect_test "parse: case insensitive side" =
  print_parse "buy 1 AAPL 100 150.00";
  print_parse "Buy 2 AAPL 100 150.00";
  [%expect
    {|
    BUY 1 AAPL 100@$150.00 DAY
    BUY 2 AAPL 100@$150.00 DAY
    |}]
;;

let%expect_test "parse: with IOC time-in-force" =
  print_parse "BUY 1 AAPL 100 150.00 IOC";
  [%expect {| BUY 1 AAPL 100@$150.00 IOC |}]
;;

let%expect_test "parse: with explicit DAY" =
  print_parse "SELL 2 AAPL 200 151.00 DAY";
  [%expect {| SELL 2 AAPL 200@$151.00 DAY |}]
;;

let%expect_test "parse error: 'as <name>' is no longer supported" =
  (* Participant identity comes from the login session, not the command text,
     so a trailing "as <name>" is rejected. *)
  print_parse "BUY 1 AAPL 100 150.00 as Alice";
  [%expect {| ERROR: unknown time-in-force: as (expected DAY or IOC) |}]
;;

let%expect_test "parse error: trailing arguments after TIF" =
  print_parse "SELL 2 GOOG 75 2800.50 IOC as Bob";
  [%expect {| ERROR: unexpected trailing arguments: as Bob |}]
;;

let%expect_test "parse: symbol is uppercased" =
  print_parse "BUY 1 aapl 100 150.00";
  [%expect {| BUY 1 aapl 100@$150.00 DAY |}]
;;

let%expect_test "parse: extra whitespace is ignored" =
  print_parse "  BUY 3   AAPL   100   150.00  ";
  [%expect {| BUY 3 AAPL 100@$150.00 DAY |}]
;;

let%expect_test "parse: price with dollar sign" =
  print_parse "BUY 1 AAPL 100 $150.25";
  [%expect {| BUY 1 AAPL 100@$150.25 DAY |}]
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
  print_parse "HOLD 1 AAPL 100 150.00";
  [%expect {| ERROR: unknown command: HOLD (expected BUY or SELL) |}]
;;

let%expect_test "parse error: missing fields" =
  print_parse "BUY 2 AAPL";
  print_parse "BUY 3";
  [%expect
    {|
    ERROR: expected: BUY|SELL <client_order_id> <symbol> <size> <price> [DAY|IOC]
    ERROR: expected: BUY|SELL <client_order_id> <symbol> <size> <price> [DAY|IOC]
    |}]
;;

let%expect_test "parse error: invalid size" =
  print_parse "BUY 1 AAPL abc 150.00";
  print_parse "BUY 2 AAPL 0 150.00";
  print_parse "BUY 3 AAPL -5 150.00";
  [%expect
    {|
    ERROR: invalid size: abc
    ERROR: size must be positive
    ERROR: size must be positive
    |}]
;;

let%expect_test "parse error: invalid price" =
  print_parse "BUY 1 AAPL 100 xyz";
  [%expect
    {|
    ERROR: invalid price: xyz
    exception: (Invalid_argument "Float.of_string xyz")
    |}]
;;

let%expect_test "parse error: unknown time-in-force" =
  print_parse "BUY 2 AAPL 100 150.00 QQQ";
  [%expect {| ERROR: unknown time-in-force: QQQ (expected DAY or IOC) |}]
;;

(* --- Exchange_command.parse --- *)

let%expect_test "SUBMIT via Exchange_command carries the client order id" =
  let req =
    match Exchange_command.parse "BUY 7 AAPL 100 150.00" |> ok_exn with
    | Submit r -> r
    | Book _ | Subscribe _ -> failwith "Expected order"
  in
  print_endline
    [%string "client_order_id=%{req.client_order_id#Client_order_id}"];
  [%expect {| client_order_id=7 |}]
;;

(* 8c Additional Tests *)

let%expect_test "BOOK with a symbol argument" =
  let symbol =
    match Exchange_command.parse "BOOK AAPL" |> ok_exn with
    | Book s -> s
    | Submit _ | Subscribe _ -> failwith "Expected order"
  in
  print_endline [%string "symbol=%{symbol#Symbol}"];
  [%expect {| symbol=AAPL |}]
;;

let%expect_test "SUBSCRIBE with case-insensitive input" =
  let symbol =
    match Exchange_command.parse "subsCribe AAPL" |> ok_exn with
    | Subscribe s -> s
    | Submit _ | Book _ -> failwith "Expected order"
  in
  print_endline [%string "symbol=%{symbol#Symbol}"];
  [%expect {| symbol=AAPL |}]
;;

(* --- Event formatting --- *)

let%expect_test "format_event: all event types" =
  let events =
    [ Exchange_event.Order_accept
        { order_id = Order_id.of_string "1"
        ; participant = Participant.of_string "Alice"
        ; request =
            { client_order_id = Client_order_id.For_testing.of_int 1
            ; symbol = Symbol.of_string "AAPL"
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
        ; aggressor_client_order_id = Client_order_id.For_testing.of_int 1
        ; aggressor_order_id = Order_id.of_string "2"
        ; aggressor_participant = Participant.of_string "Alice"
        ; aggressor_side = Buy
        ; resting_client_order_id = Client_order_id.For_testing.of_int 2
        ; resting_order_id = Order_id.of_string "1"
        ; resting_participant = Participant.of_string "Bob"
        }
    ; Order_cancel
        { order_id = Order_id.of_string "3"
        ; client_order_id = Client_order_id.For_testing.of_int 3
        ; participant = Participant.of_string "Charlie"
        ; symbol = Symbol.of_string "TSLA"
        ; remaining_size = Size.of_int 50
        ; reason = Ioc_remainder
        }
    ; Order_reject
        { participant = Participant.of_string "Alice"
        ; request =
            { client_order_id = Client_order_id.For_testing.of_int 1
            ; symbol = Symbol.of_string "GOOG"
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
    FILL fill_id=1 AAPL $150.00 x100 aggressor=1|2(Alice) BUY resting=2|1(Bob)
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
    ~participant:Harness.bob
    t
    (Harness.sell ~price_cents:15000 ());
  (* Parse a buy command from text and submit it as Alice *)
  let request =
    Protocol.parse_command "BUY 2 AAPL 100 150.00"
    |> Result.map_error ~f:Error.of_string
    |> ok_exn
  in
  let events =
    Matching_engine.submit
      (Harness.engine t)
      ~participant:Harness.alice
      request
  in
  print_endline (Protocol.format_events events);
  [%expect
    {|
    ACCEPTED id=1 AAPL SELL 100@$150.00 DAY
    BBO AAPL bid=- ask=$150.00 x100
    ACCEPTED id=2 AAPL BUY 100@$150.00 DAY
    FILL fill_id=1 AAPL $150.00 x100 aggressor=2|2(Alice) BUY resting=101|1(Bob)
    TRADE AAPL $150.00 x100
    BBO AAPL bid=- ask=-
    |}]
;;
