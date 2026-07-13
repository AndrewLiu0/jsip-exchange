(** Benchmarks for the order book and matching engine.

    Run with: dune exec lib/order_book/bench/bench_order_book.exe -- existing
    -ascii -quota 5

    These benchmarks measure the core operations of the exchange and are
    designed to give you meaningful feedback on the performance of the system
    and the effect of any optimizations you make.

    {2 How to read the results}

    Core_bench reports time per operation in nanoseconds. Lower is better.
    Focus on:
    - [find_match]: the hot path — called on every incoming order
    - [submit_ioc_cross]: end-to-end order submission with a fill
    - [add/remove]: book mutation performance
    - [best_price]: how fast you can query the BBO

    {2 Tips for meaningful benchmarks}

    {ul
     {- Use [-quota 5] or higher for stable results (5 seconds per bench). }
     {- Run on a quiet machine (no heavy background processes). }
     {- Compare before/after by saving results:

       {v
          dune exec lib/order_book/bench/bench_order_book.exe -- existing -ascii -quota 5 > before.txt
          # ... make your changes ...
          dune exec lib/order_book/bench/bench_order_book.exe -- existing -ascii -quota 5 > after.txt
          diff before.txt after.txt
       v}
    }
    } *)

open! Core
open Core_bench
open Jsip_types
open Jsip_order_book

(* ---------------------------------------------------------------- *)
(* Setup helpers *)
(* ---------------------------------------------------------------- *)

let aapl = Symbol_id.of_int_exn 0
let alice = Participant.of_string "Alice"
let bob = Participant.of_string "Bob"

(** Build a book with [n] resting sell orders at prices 1..n (in cents). This
    gives a realistic spread of prices for benchmarking find_match and
    best_price queries. *)
let book_with_n_asks ?(min_price = 10_000) n =
  let book = Order_book.create aapl in
  let gen = Order_id.Generator.create () in
  for i = 1 to n do
    let order =
      Order.create
        { client_order_id = Client_order_id.For_testing.of_int i
        ; symbol = aapl
        ; side = Sell
        ; price = Price.of_int_cents (min_price + i)
        ; size = Size.of_int 100
        ; time_in_force = Day
        }
        ~order_id:(Order_id.Generator.next gen)
        ~participant:bob
    in
    Order_book.add book order
  done;
  book, gen
;;

(** Build a book with [n] resting sells all at the same price -- the worst
    case for snapshot aggregation, which must fold that whole queue into one
    level. [book_with_n_asks] can't exercise this: it puts every order at a
    distinct price. *)
let book_with_n_same_price_asks n =
  let book = Order_book.create aapl in
  let gen = Order_id.Generator.create () in
  let client_id_gen = Client_order_id.Generator.create () in
  for _ = 1 to n do
    let order =
      Order.create
        { client_order_id = Client_order_id.Generator.generate client_id_gen
        ; symbol = aapl
        ; side = Sell
        ; price = Price.of_int_cents 15_000
        ; size = Size.of_int 100
        ; time_in_force = Day
        }
        ~order_id:(Order_id.Generator.next gen)
        ~participant:bob
    in
    Order_book.add book order
  done;
  book
;;

(** Build a matching engine with [n] resting sells on AAPL. *)
let engine_with_n_asks ?(min_price = 10_000) n =
  let engine = Matching_engine.create ~num_symbols:1 in
  for i = 1 to n do
    ignore
      (Matching_engine.submit
         engine
         ~participant:bob
         { client_order_id = Client_order_id.For_testing.of_int i
         ; symbol = aapl
         ; side = Sell
         ; price = Price.of_int_cents (min_price + i)
         ; size = Size.of_int 100
         ; time_in_force = Day
         }
       : Exchange_event.t list)
  done;
  engine
;;

(* ---------------------------------------------------------------- *)
(* Order_book micro-benchmarks *)
(* ---------------------------------------------------------------- *)

let bench_find_match ~n =
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  (* Incoming buy at a price that matches the best ask *)
  let incoming =
    Order.create
      { client_order_id = Client_order_id.For_testing.of_int (n + 1)
      ; symbol = aapl
      ; side = Buy
      ; price = Price.of_int_cents (min_price + n)
      ; size = Size.of_int 100
      ; time_in_force = Ioc
      }
      ~order_id:(Order_id.Generator.next gen)
      ~participant:alice
  in
  Bench.Test.create ~name:[%string "find_match (n=%{n#Int})"] (fun () ->
    ignore (Order_book.find_match book incoming : Order.t option))
;;

let bench_find_match_no_cross ~n =
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  (* Incoming buy at a price below all asks — no match possible *)
  let incoming =
    Order.create
      { client_order_id = Client_order_id.For_testing.of_int (n + 1)
      ; symbol = aapl
      ; side = Buy
      ; price = Price.of_int_cents (min_price - 1)
      ; size = Size.of_int 100
      ; time_in_force = Ioc
      }
      ~order_id:(Order_id.Generator.next gen)
      ~participant:alice
  in
  Bench.Test.create ~name:[%string "find_match_miss (n=%{n#Int})"] (fun () ->
    ignore (Order_book.find_match book incoming : Order.t option))
;;

let bench_best_bid_offer ~n =
  let book, _gen = book_with_n_asks n in
  Bench.Test.create ~name:[%string "best_bid_offer (n=%{n#Int})"] (fun () ->
    ignore (Order_book.best_bid_offer book : Bbo.t))
;;

let bench_add_remove ~n =
  (* Pre-build the book, then measure add+remove cycle *)
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  let order =
    Order.create
      { client_order_id = Client_order_id.For_testing.of_int 1
      ; symbol = aapl
      ; side = Sell
      ; price = Price.of_int_cents (min_price + 500)
      ; size = Size.of_int 100
      ; time_in_force = Day
      }
      ~order_id:(Order_id.Generator.next gen)
      ~participant:alice
  in
  let oid = Order.order_id order in
  Bench.Test.create ~name:[%string "add+remove (n=%{n#Int})"] (fun () ->
    Order_book.add book order;
    Order_book.remove book oid)
;;

let bench_snapshot ~n =
  let book = book_with_n_same_price_asks n in
  Bench.Test.create
    ~name:[%string "snapshot_same_price (n=%{n#Int})"]
    (fun () -> ignore (Order_book.snapshot book : Book.t))
;;

(* ---------------------------------------------------------------- *)
(* Matching engine end-to-end benchmarks *)
(* ---------------------------------------------------------------- *)

let bench_submit_ioc_cross ~n =
  (* Measure submitting an IOC order that crosses the best ask. This is the
     most common hot path: order in, fill out. We re-seed a resting order
     after each iteration to keep the book state consistent.

     Client order ids are never reused within an engine's lifetime (see
     [Matching_engine.submit]), so each iteration must submit under a fresh
     id — reusing one measures the duplicate-reject path instead. *)
  let min_price = 10_000 in
  let max_price = 20_000 in
  let engine = engine_with_n_asks ~min_price n in
  let next_price = ref (min_price + 1) in
  let next_client_id = ref n in
  let fresh_client_id () =
    incr next_client_id;
    Client_order_id.For_testing.of_int !next_client_id
  in
  Bench.Test.create
    ~name:[%string "submit_ioc_cross (n=%{n#Int})"]
    (fun () ->
       let events =
         Matching_engine.submit
           engine
           ~participant:alice
           { client_order_id = fresh_client_id ()
           ; symbol = aapl
           ; side = Buy
           ; price = Price.of_int_cents max_price
           ; size = Size.of_int 100
           ; time_in_force = Ioc
           }
       in
       ignore (events : Exchange_event.t list);
       (* Re-seed: add back a resting sell to replace the one we consumed *)
       ignore
         (Matching_engine.submit
            engine
            ~participant:bob
            { client_order_id = fresh_client_id ()
            ; symbol = aapl
            ; side = Sell
            ; price = Price.of_int_cents !next_price
            ; size = Size.of_int 100
            ; time_in_force = Day
            }
          : Exchange_event.t list);
       next_price := !next_price + 1;
       if !next_price > max_price then next_price := min_price + 1)
;;

let bench_submit_ioc_no_match ~n =
  (* A non-marketable IOC is accepted then cancelled, never resting, so the
     book is unchanged across iterations — only the id must be fresh. *)
  let min_price = 10_000 in
  let engine = engine_with_n_asks ~min_price n in
  let next_client_id = ref n in
  Bench.Test.create ~name:[%string "submit_ioc_miss (n=%{n#Int})"] (fun () ->
    incr next_client_id;
    ignore
      (Matching_engine.submit
         engine
         ~participant:alice
         { client_order_id =
             Client_order_id.For_testing.of_int !next_client_id
         ; symbol = aapl
         ; side = Buy
         ; price = Price.of_int_cents (min_price - 1)
         ; size = Size.of_int 100
         ; time_in_force = Ioc
         }
       : Exchange_event.t list))
;;

let bench_submit_sweep ~n =
  (* Measure an aggressive order that sweeps through the entire book.
     Re-seeds the book after each sweep. This is worst-case: every resting
     order is visited and filled. *)
  let engine = ref (engine_with_n_asks n) in
  Bench.Test.create ~name:[%string "submit_sweep_%{n#Int}_levels"] (fun () ->
    ignore
      (Matching_engine.submit
         !engine
         ~participant:alice
         { client_order_id = Client_order_id.For_testing.of_int 1
         ; symbol = aapl
         ; side = Buy
         ; price = Price.of_int_cents 99_999
         ; size = Size.of_int (n * 100)
         ; time_in_force = Ioc
         }
       : Exchange_event.t list);
    (* Re-seed entire book *)
    engine := engine_with_n_asks n)
;;

(* ---------------------------------------------------------------- *)
(* Deep-level benchmarks: all resting orders at ONE price *)
(* ---------------------------------------------------------------- *)

(* The distinct-price fixtures above put one order per price level, so any
   per-level fold looks O(1) no matter how big the book. These benchmarks
   pile [n] orders onto a single level — the shape the book-fill scenario
   produces — where [queue_to_level]'s fold visits all [n] orders per call. *)

let deep_sizes = [ 100; 1_000; 10_000 ]
let deep_level_price = 15_000

(** Engine with [n] resting sells all at [deep_level_price]: one giant ask
    level, empty bid side. Client ids 1..n are used (by [bob]). *)
let engine_with_n_same_price_asks n =
  let engine = Matching_engine.create ~num_symbols:1 in
  for i = 1 to n do
    ignore
      (Matching_engine.submit
         engine
         ~participant:bob
         { client_order_id = Client_order_id.For_testing.of_int i
         ; symbol = aapl
         ; side = Sell
         ; price = Price.of_int_cents deep_level_price
         ; size = Size.of_int 100
         ; time_in_force = Day
         }
       : Exchange_event.t list)
  done;
  engine
;;

let bench_best_bid_offer_deep ~n =
  let book = book_with_n_same_price_asks n in
  Bench.Test.create
    ~name:[%string "best_bid_offer_deep (n=%{n#Int})"]
    (fun () -> ignore (Order_book.best_bid_offer book : Bbo.t))
;;

let bench_submit_rest_deep ~n =
  (* The book-fill hot path: a non-marketable Day order submitted while one
     huge level rests on the opposite side. [Matching_engine.submit] computes
     the BBO before and after the order is added, folding that whole level
     both times, so this should scale with [n] until the book is fixed. *)
  let engine = engine_with_n_same_price_asks n in
  let next_client_id = ref n in
  Bench.Test.create
    ~name:[%string "submit_rest_deep (n=%{n#Int})"]
    (fun () ->
       (* Submit-then-cancel keeps the book at [n] resting orders across
          iterations; the cancel is part of the measured work. *)
       incr next_client_id;
       let client_order_id =
         Client_order_id.For_testing.of_int !next_client_id
       in
       ignore
         (Matching_engine.submit
            engine
            ~participant:alice
            { client_order_id
            ; symbol = aapl
            ; side = Buy
            ; price = Price.of_int_cents (deep_level_price - 1)
            ; size = Size.of_int 100
            ; time_in_force = Day
            }
          : Exchange_event.t list);
       ignore
         (Matching_engine.cancel engine alice client_order_id
          : Exchange_event.t list))
;;

(* ---------------------------------------------------------------- *)
(* Symbol-lookup benchmarks (Part 4, Ex 2, re-measured for Ex 4) *)
(* ---------------------------------------------------------------- *)

(* Symbol counts get their own sweep: [sizes] above counts resting orders,
   whereas the symbol lookup only becomes visible with many symbols. *)
let symbol_counts = [ 10; 100; 1_000; 10_000 ]

(** Engine trading [k] symbols, all books empty: [Matching_engine.book] is
    then a pure symbol-resolution measurement, not buried under matching
    work. The benchmarks in [tests] are all single-symbol, so they never
    stress this lookup.

    In Ex 2 this measured hash-then-index: the request carried a string
    symbol and the engine hashed it back to an id (baseline in
    [symbol_lookup_after.txt] at the repo root). Since Ex 4 the wire carries
    the id itself, so the same [Matching_engine.book] call is a bounds check
    plus an array read — compare against the baseline to see what pushing the
    id across the wire bought ([symbol_lookup_by_id.txt]). *)
let bench_symbol_lookup ~k =
  let engine = Matching_engine.create ~num_symbols:k in
  (* Probes prebuilt outside the thunks. The miss is one past the last valid
     id, i.e. out of range. *)
  let hit = Symbol_id.of_int_exn (k / 2) in
  let miss = Symbol_id.of_int_exn k in
  [ Bench.Test.create
      ~name:[%string "book_hit (symbols=%{k#Int})"]
      (fun () ->
         ignore (Matching_engine.book engine hit : Order_book.t option))
  ; Bench.Test.create
      ~name:[%string "book_miss (symbols=%{k#Int})"]
      (fun () ->
         ignore (Matching_engine.book engine miss : Order_book.t option))
  ]
;;

(* ---------------------------------------------------------------- *)
(* Allocation measurement *)
(* ---------------------------------------------------------------- *)

let bench_find_match_alloc ~n =
  let min_price = 10_000 in
  let book, gen = book_with_n_asks ~min_price n in
  let incoming =
    Order.create
      { client_order_id = Client_order_id.For_testing.of_int (n + 1)
      ; symbol = aapl
      ; side = Buy
      ; price = Price.of_int_cents (min_price + n)
      ; size = Size.of_int 100
      ; time_in_force = Ioc
      }
      ~order_id:(Order_id.Generator.next gen)
      ~participant:alice
  in
  (* Measure minor-heap allocations *)
  let measure_alloc f =
    Gc.compact ();
    let before = (Gc.stat ()).minor_words in
    for _ = 1 to 1000 do
      f ()
    done;
    let after = (Gc.stat ()).minor_words in
    (after -. before) /. 1000.0
  in
  let words_per_call =
    measure_alloc (fun () ->
      ignore (Order_book.find_match book incoming : Order.t option))
  in
  Bench.Test.create
    ~name:
      (sprintf "find_match_alloc (n=%d, %.1f words/call)" n words_per_call)
    (fun () -> ignore (Order_book.find_match book incoming : Order.t option))
;;

(* ---------------------------------------------------------------- *)
(* Main *)
(* ---------------------------------------------------------------- *)

let sizes = [ 10; 50; 100; 500 ]

let tests =
  List.concat
    [ (* Order book micro-benchmarks at various sizes *)
      List.map sizes ~f:(fun n -> bench_find_match ~n)
    ; List.map sizes ~f:(fun n -> bench_find_match_no_cross ~n)
    ; List.map sizes ~f:(fun n -> bench_best_bid_offer ~n)
    ; [ bench_add_remove ~n:100 ]
    ; (* Matching engine end-to-end *)
      List.map sizes ~f:(fun n -> bench_submit_ioc_cross ~n)
    ; List.map sizes ~f:(fun n -> bench_submit_ioc_no_match ~n)
    ; List.map [ 10; 50; 100 ] ~f:(fun n -> bench_submit_sweep ~n)
    ; (* Allocation awareness *)
      [ bench_find_match_alloc ~n:100 ]
    ]
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"JSIP order-book benchmarks"
       [ "existing", Bench.make_command tests
       ; ( "snapshot"
         , Bench.make_command
             (List.map sizes ~f:(fun n -> bench_snapshot ~n)) )
       ; ( "symbol-lookup"
         , Bench.make_command
             (List.concat_map symbol_counts ~f:(fun k ->
                bench_symbol_lookup ~k)) )
       ; ( "deep-level"
         , Bench.make_command
             (List.concat
                [ List.map deep_sizes ~f:(fun n ->
                    bench_best_bid_offer_deep ~n)
                ; List.map deep_sizes ~f:(fun n -> bench_submit_rest_deep ~n)
                ]) )
       ])
;;
