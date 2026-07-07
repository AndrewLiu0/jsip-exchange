(** Glue that boots a scenario into a running exchange + ecosystem of bots. *)

open! Core
open! Async

(** Boot the exchange on [port], spin up the oracle/news/bots described by
    [config], and return a deferred that resolves only when the server is
    closed. The deferred for each bot's tick loop is leaked via
    [don't_wait_for].

    [http_port] additionally serves the browser dashboard (and RPCs over
    websocket) on that port — the way to watch the exchange's health while a
    scenario runs. *)
val run
  :  ?http_port:int
  -> Scenario_config.t
  -> port:int
  -> seed:int
  -> unit Deferred.t
