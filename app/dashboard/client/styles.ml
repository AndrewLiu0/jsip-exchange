(** Style tokens for the dashboard, kept in one module so spacing and color
    stay consistent across panes. Each value is a whole [style] attribute
    (one per node — two [style] attrs on one node would clobber each other);
    colors and sizes reference the CSS custom properties defined on [:root]
    in [index.html]. ppx_html's [style="..."] syntax is off-limits here (it
    desugars to ppx_css, which this project doesn't use), hence
    {!Vdom.Attr.create}. *)

open! Core
open Virtual_dom

let style s = Vdom.Attr.create "style" s

let page =
  style
    "min-height: 100vh; padding: var(--space-lg); display: flex; \
     flex-direction: column; gap: var(--space-lg)"
;;

let header =
  style
    "font-size: var(--font-size-md); font-weight: 600; letter-spacing: 0; \
     color: var(--color-text-primary)"
;;

let header_note =
  style
    "font-size: var(--font-size-xs); color: var(--color-text-tertiary); \
     margin-left: var(--space-sm)"
;;

let grid =
  style
    "display: grid; grid-template-columns: repeat(auto-fit, minmax(340px, \
     1fr)); gap: var(--space-md); align-items: start"
;;

(* Tier-1 panel: page-level sections are unframed bands elsewhere, but the
   four metric panes are genuinely repeated framed tools, so they get the
   panel treatment. *)
let pane =
  style
    "background: var(--color-bg-1); border: 1px solid \
     var(--color-border-1); border-radius: var(--radius-md); padding: \
     var(--space-md); display: flex; flex-direction: column; gap: \
     var(--space-sm)"
;;

let pane_title =
  style
    "margin: 0; font-size: var(--font-size-xs); font-weight: 600; \
     text-transform: uppercase; letter-spacing: 0; color: \
     var(--color-text-tertiary)"
;;

let stat_value =
  style
    "font-family: var(--font-mono); font-variant-numeric: tabular-nums; \
     font-size: var(--font-size-xl); color: var(--color-text-primary)"
;;

let stat_row =
  style
    "display: flex; justify-content: space-between; gap: var(--space-md); \
     font-family: var(--font-mono); font-variant-numeric: tabular-nums; \
     font-size: var(--font-size-sm)"
;;

let stat_label = style "color: var(--color-text-secondary)"
let stat_number = style "color: var(--color-text-primary); text-align: right"

let muted =
  style "color: var(--color-text-tertiary); font-size: var(--font-size-xs)"
;;

let section_label =
  style
    "color: var(--color-text-tertiary); font-size: var(--font-size-xs); \
     margin-top: var(--space-xs)"
;;

let sparkline_box =
  style
    "width: 100%; height: 48px; margin-top: var(--space-xs); background: \
     var(--color-bg-0); border: 1px solid var(--color-border-1); \
     border-radius: var(--radius-md)"
;;

let footer =
  style
    "margin-top: auto; color: var(--color-text-tertiary); font-size: \
     var(--font-size-xs); font-variant-numeric: tabular-nums"
;;
