(** Dashboard entry point. Starts the Bonsai app immediately (so the page
    renders its waiting state without blocking on the network), then in the
    background connects to the exchange over the page's own websocket origin,
    dispatches the exchange-stats pipe RPC, and forwards snapshots into the
    pipe the app is draining.

    Connection errors are logged to the browser console rather than raised:
    exceptions are slow and invisible in js_of_ocaml, and the page's
    "connecting…" state is already an honest rendering of the failure. *)

open! Core
open Async_kernel
open Async_js
open Jsip_stats

let () =
  Async_js.init ();
  let snapshots, snapshots_writer = Pipe.create () in
  don't_wait_for
    (match%bind Rpc.Connection.client () with
     | Error error ->
       eprint_s
         [%message
           "failed to connect to the exchange websocket" (error : Error.t)];
       return ()
     | Ok connection ->
       (match%bind
          Rpc.Pipe_rpc.dispatch Stats_protocol.stats_rpc connection ()
        with
        | Error error | Ok (Error error) ->
          eprint_s
            [%message "exchange-stats dispatch failed" (error : Error.t)];
          return ()
        | Ok (Ok (pipe, (_ : Rpc.Pipe_rpc.Metadata.t))) ->
          Pipe.transfer_id pipe snapshots_writer));
  Bonsai_web.Start.start (Web_app.app ~snapshots)
;;
