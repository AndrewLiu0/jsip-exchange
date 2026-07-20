(** The replay driver (Part 4, Exercise 6): pumps {!Workload} actions
    straight into a {!Jsip_order_book.Matching_engine} in a plain synchronous
    loop — no Async, no RPC — and reports where the run's time and events
    went.

    This is the load harness the profiling exercise attaches [perf] to: run
    it flat out and the process spends its cycles in engine code rather than
    scheduler sleep.

    The report answers two different questions and both matter:

    - {b Was the workload what the config promised?} Event counts (fills,
      cancels by reason, rejects) and the periodic progress lines (book
      depth, BBO, GC live words) verify steady state — a growing book or a
      marching BBO means the run is not measuring one consistent thing.
    - {b How fast was the engine?} Wall time, actions/sec, and per-call
      latency percentiles, measured around the engine call only (generator
      and bookkeeping time is excluded from latencies but included in wall
      time — which is why actions/sec will not match the microbenchmarks'
      inverse Time/Run).

    Run via the perf binary:
    {[
      dune exec performance/bin/main.exe -- replay -preset balanced \
        -num-actions 1000000 -seed 0
    ]} *)

open! Core

val command : Command.t
