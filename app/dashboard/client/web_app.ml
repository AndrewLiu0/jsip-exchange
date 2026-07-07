(** The Bonsai layer of the dashboard: a state machine holding the pure
    {!Jsip_dashboard.Controller}, fed by draining the snapshot pipe on
    activation (the same shape as [Jsip_monitor.Term_app]), and pure render
    functions from {!Controller.Display} to vdom. *)

open! Core
open Async_kernel
open Bonsai_web
open Bonsai.Let_syntax
open Jsip_types
open Jsip_stats
open Jsip_dashboard

module Action = struct
  type t = Feed_snapshot of Snapshot.t [@@deriving sexp_of]
end

let format_count count = Int.to_string_hum ~delimiter:',' count

let format_span = function
  | None -> "—"
  | Some span -> Time_ns.Span.to_string_hum ~decimals:0 span
;;

(* Reusable framed pane; the [view] convention makes it callable as
   [<Pane.view ~title:...>] from ppx_html. *)
module Pane = struct
  let view ~title children =
    {%html|
      <section %{Styles.pane}>
        <h2 %{Styles.pane_title}>#{title}</h2>
        *{children}
      </section>
    |}
  ;;
end

(* A single-series monochrome sparkline. One series means no legend (the pane
   title names it); the y-axis is autoscaled to the window's min/max because
   the interesting signal is the *shape* of memory growth, not its absolute
   level. Hand-rolled SVG: ppx_html has no svg tags, so the nodes are built
   with [Vdom.Node.create_svg]. *)
let render_sparkline (series : (Time_ns.Alternate_sexp.t * int) list) =
  let values = List.map series ~f:snd in
  match
    values, List.min_elt values ~compare, List.max_elt values ~compare
  with
  | ([] | [ _ ]), _, _ | _, None, _ | _, _, None ->
    {%html|<div %{Styles.muted}>collecting…</div>|}
  | values, Some min_value, Some max_value ->
    let width = 300. in
    let height = 48. in
    let pad = 3. in
    let count = List.length values in
    let points =
      List.mapi values ~f:(fun i value ->
        let x =
          (Float.of_int i
           /. Float.of_int (count - 1)
           *. (width -. (2. *. pad)))
          +. pad
        in
        let y =
          if min_value = max_value
          then height /. 2.
          else (
            let fraction =
              Float.of_int (value - min_value)
              /. Float.of_int (max_value - min_value)
            in
            height -. pad -. (fraction *. (height -. (2. *. pad))))
        in
        sprintf "%.1f,%.1f" x y)
      |> String.concat ~sep:" "
    in
    let svg_attr name value = Vdom.Attr.create name value in
    Vdom.Node.create_svg
      "svg"
      ~attrs:
        [ svg_attr "viewBox" [%string "0 0 %{width#Float} %{height#Float}"]
        ; svg_attr "preserveAspectRatio" "none"
        ; Styles.sparkline_box
        ]
      [ Vdom.Node.create_svg
          "polyline"
          ~attrs:
            [ svg_attr "points" points
            ; svg_attr "fill" "none"
            ; svg_attr "stroke" "var(--color-accent)"
            ; svg_attr "stroke-width" "2"
            ; svg_attr "vector-effect" "non-scaling-stroke"
            ]
          []
      ]
;;

let render_memory_pane
  ~(series : (Time_ns.Alternate_sexp.t * int) list)
  ~(latest : Snapshot.Memory.t option)
  =
  let stat_row label value =
    {%html|
      <div %{Styles.stat_row}>
        <span %{Styles.stat_label}>#{label}</span>
        <span %{Styles.stat_number}>#{value}</span>
      </div>
    |}
  in
  let body =
    match latest with
    | None -> [ {%html|<div %{Styles.muted}>waiting for snapshots…</div>|} ]
    | Some memory ->
      [ {%html|<div %{Styles.stat_value}>#{format_count memory.live_words}</div>|}
      ; {%html|<div %{Styles.muted}>live words — last 60s</div>|}
      ; render_sparkline series
      ; stat_row "heap words" (format_count memory.heap_words)
      ; stat_row "top heap words" (format_count memory.top_heap_words)
      ; stat_row "major collections" (format_count memory.major_collections)
      ; stat_row "compactions" (format_count memory.compactions)
      ]
  in
  {%html|<Pane.view ~title:%{"memory"}> *{body} </>|}
;;

let render_latency_pane ~title (stats : Controller.Latency_stats.t) =
  let { Controller.Latency_stats.p50
      ; p90
      ; p99
      ; window_sample_count
      ; window_total_count
      }
    =
    stats
  in
  let percentile_row label span =
    {%html|
      <div %{Styles.stat_row}>
        <span %{Styles.stat_label}>#{label}</span>
        <span %{Styles.stat_number}>#{format_span span}</span>
      </div>
    |}
  in
  let sample_note =
    (* Under load the server caps how many raw samples it ships, so the
       percentiles may be computed over a subset; surface both counts so a
       reader knows how much to trust them. *)
    [%string
      "%{format_count window_sample_count} sampled of %{format_count \
       window_total_count}"]
  in
  let body =
    [ percentile_row "p50" p50
    ; percentile_row "p90" p90
    ; percentile_row "p99" p99
    ; {%html|<div %{Styles.muted}>#{sample_note}</div>|}
    ]
  in
  {%html|<Pane.view ~title:%{title}> *{body} </>|}
;;

let render_occupancy_pane (occupancy : Snapshot.Pipe_occupancy.t option) =
  let body =
    match occupancy with
    | None -> [ {%html|<div %{Styles.muted}>waiting for snapshots…</div>|} ]
    | Some { request_queue; market_data; audit; sessions } ->
      let row label value =
        {%html|
          <div %{Styles.stat_row}>
            <span %{Styles.stat_label}>#{label}</span>
            <span %{Styles.stat_number}>#{value}</span>
          </div>
        |}
      in
      let section label rows =
        match rows with
        | [] -> [ row label "none" ]
        | rows ->
          {%html|<div %{Styles.section_label}>#{label}</div>|} :: rows
      in
      let counts_to_string counts =
        counts |> List.map ~f:Int.to_string |> String.concat ~sep:", "
      in
      List.concat
        [ [ row "request queue" (Int.to_string request_queue) ]
        ; section
            "market data"
            (List.map market_data ~f:(fun (symbol, counts) ->
               row (Symbol.to_string symbol) (counts_to_string counts)))
        ; section
            "audit"
            (List.mapi audit ~f:(fun i count ->
               row [%string "subscriber %{i#Int}"] (Int.to_string count)))
        ; section
            "sessions"
            (List.map sessions ~f:(fun (participant, count) ->
               row (Participant.to_string participant) (Int.to_string count)))
        ]
  in
  {%html|<Pane.view ~title:%{"pipe occupancy"}> *{body} </>|}
;;

let render_page (display : Controller.Display.t) =
  let { Controller.Display.memory_series
      ; latest_memory
      ; submit
      ; cancel
      ; occupancy
      ; snapshots_received
      }
    =
    display
  in
  let liveness =
    match snapshots_received with
    | 0 -> "connecting to the exchange…"
    | n -> [%string "%{format_count n} snapshots received"]
  in
  {%html|
    <div %{Styles.page}>
      <header %{Styles.header}>
        jsip exchange
        <span %{Styles.header_note}>process monitor</span>
      </header>
      <div %{Styles.grid}>
        %{render_memory_pane ~series:memory_series ~latest:latest_memory}
        %{render_latency_pane ~title:"submit latency" submit}
        %{render_latency_pane ~title:"cancel latency" cancel}
        %{render_occupancy_pane occupancy}
      </div>
      <footer %{Styles.footer}>#{liveness}</footer>
    </div>
  |}
;;

(* On activation, spawn a background task draining the snapshot pipe into the
   state machine — a direct port of [Term_app.drain_events_on_activate].
   [handle_non_dom_event_exn] is how an effect is scheduled from outside a
   DOM event handler. *)
let drain_snapshots_on_activate snapshots inject =
  Effect.of_thunk (fun () ->
    don't_wait_for
      (Pipe.iter_without_pushback snapshots ~f:(fun snapshot ->
         Vdom.Effect.Expert.handle_non_dom_event_exn
           (inject (Action.Feed_snapshot snapshot)))))
;;

let app ~(snapshots : Snapshot.t Pipe.Reader.t) (local_ graph) =
  let controller, inject =
    Bonsai.state_machine
      ~default_model:(Controller.create ())
      ~apply_action:(fun _ctx model (Action.Feed_snapshot snapshot) ->
        Controller.feed_snapshot model snapshot)
      graph
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr inject in
       drain_snapshots_on_activate snapshots inject)
    graph;
  let%arr controller in
  render_page (Controller.display controller)
;;
