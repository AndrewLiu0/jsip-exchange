open! Core
open! Async
open Jsip_bot_runtime
open Jsip_types

module Per_symbol_data = struct
  type t =
    { last_3 : float Queue.t
    ; mutable sum_latencies : float
    ; mutable num_samples : int
    }
end

module RCConfig = struct
  type t =
    { participant : Participant.t
    ; request_interval : int
    ; report_interval : int
    ; symbols : Symbol.t list
    ; book_query : Symbol.t -> Book.t option Deferred.t
    ; latency_data : Per_symbol_data.t Symbol.Table.t
    ; mutable ticks_since_start : int
    }
end

module Timer = struct
  type t =
    { start_time : Time_ns.t
    ; mutable end_time : Time_ns.t option
    }

  let start_timer () = { start_time = Time_ns.now (); end_time = None }
  let stop_timer t = t.end_time <- Some (Time_ns.now ())

  let get_time_elapsed t =
    match t.end_time with
    | None -> Or_error.error_string "Timer has not been stopped"
    | Some time ->
      Or_error.return (Time_ns.diff time t.start_time |> Time_ns.Span.to_ms)
  ;;
end

module Latency_report = struct
  type t =
    { most_recent_latency_ms : float option
    ; avg_latency_ms : float option
    ; last_3_avg_latency_ms : float option
    }

  let format_time_ms = function
    | None -> "N/A"
    | Some ts -> Float.to_string ts ^ "ms"
  ;;

  let to_string t symbol =
    [%string
      "(%{symbol#Symbol}) most_recent_latency_ms: %{format_time_ms \
       t.most_recent_latency_ms} avg_latency_ms: %{format_time_ms \
       t.avg_latency_ms} last_3_avg_latency_ms: %{format_time_ms \
       t.last_3_avg_latency_ms}"]
  ;;
end

module T = struct
  module Config = struct
    type t = RCConfig.t
  end

  let name = "ResourceCanary"

  (* We only ever keep this many samples in the [last_3] window. *)
  let num_recent = 3

  let get_last_3_avg (queue : float Queue.t) =
    if Queue.length queue = num_recent
    then
      Some
        (Queue.fold queue ~init:0.0 ~f:(fun sum num -> sum +. num)
         /. Int.to_float num_recent)
    else None
  ;;

  let get_latest (queue : float Queue.t) = Queue.last queue

  let perform_analysis (symbol_latency_data : Per_symbol_data.t) =
    let last_3_avg_latency_ms = get_last_3_avg symbol_latency_data.last_3 in
    let most_recent_latency_ms = get_latest symbol_latency_data.last_3 in
    let avg_latency_ms =
      match symbol_latency_data.num_samples with
      | 0 -> None
      | num_samples ->
        Some (symbol_latency_data.sum_latencies /. Int.to_float num_samples)
    in
    { Latency_report.last_3_avg_latency_ms
    ; most_recent_latency_ms
    ; avg_latency_ms
    }
  ;;

  (* Records [latency] into the bounded [queue] of the most recent
     [num_recent] samples. *)
  let record_recent_latency (queue : float Queue.t) (latency : float) : unit =
    if Queue.length queue >= num_recent
    then ignore (Queue.dequeue queue : float option);
    Queue.enqueue queue latency
  ;;

  let on_start (config : Config.t) (_context : Bot_runtime.Context.t) =
    if config.request_interval <= 0
    then raise_s [%message "request_interval must be positive"];
    if config.report_interval <= 0
    then raise_s [%message "report_interval must be positive"];
    config.ticks_since_start <- 0;
    List.iter config.symbols ~f:(fun symbol ->
      Hashtbl.set
        config.latency_data
        ~key:symbol
        ~data:
          { Per_symbol_data.last_3 = Queue.create ()
          ; sum_latencies = 0.0
          ; num_samples = 0
          });
    return ()
  ;;

  let on_event
    (_config : Config.t)
    (_context : Bot_runtime.Context.t)
    (_event : Exchange_event.t)
    =
    return ()
  ;;

  let on_tick (config : Config.t) (_context : Bot_runtime.Context.t) =
    config.ticks_since_start <- config.ticks_since_start + 1;
    let%bind () =
      if config.ticks_since_start mod config.request_interval = 0
      then
        Deferred.List.iter config.symbols ~how:`Sequential ~f:(fun symbol ->
          let (latency_data : Per_symbol_data.t) =
            Hashtbl.find_exn config.latency_data symbol
          in
          let timer = Timer.start_timer () in
          let%bind (_ : Book.t option) = config.book_query symbol in
          Timer.stop_timer timer;
          let time_elapsed =
            Timer.get_time_elapsed timer |> Or_error.ok_exn
          in
          record_recent_latency latency_data.last_3 time_elapsed;
          latency_data.sum_latencies
          <- latency_data.sum_latencies +. time_elapsed;
          latency_data.num_samples <- latency_data.num_samples + 1;
          return ())
      else return ()
    in
    if config.ticks_since_start mod config.report_interval = 0
    then (
      let report_string =
        Hashtbl.fold
          config.latency_data
          ~init:[ "RESOURCE CANARY REPORT" ]
          ~f:(fun ~key:symbol ~data string_list ->
            let report = perform_analysis data in
            string_list @ [ Latency_report.to_string report symbol ])
        |> String.concat ~sep:"\n"
      in
      print_endline report_string;
      print_endline "";
      return ())
    else Deferred.unit
  ;;
end

include T

module For_testing = struct
  let create_data ~recent ~sum_latencies ~num_samples : Per_symbol_data.t =
    { last_3 = Queue.of_list recent; sum_latencies; num_samples }
  ;;

  let num_samples (data : Per_symbol_data.t) = data.num_samples
end
