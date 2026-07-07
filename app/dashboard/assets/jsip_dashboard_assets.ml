open! Core
open! Async

let handler
  ~body:(_ : Cohttp_async.Body.t)
  (_ : Socket.Address.Inet.t)
  (request : Cohttp_async.Request.t)
  =
  let respond ~content_type body =
    Cohttp_async.Server.respond_string
      ~headers:(Cohttp.Header.of_list [ "Content-Type", content_type ])
      body
  in
  match Uri.path (Cohttp.Request.uri request) with
  | "/" | "/index.html" ->
    respond
      ~content_type:"text/html; charset=utf-8"
      Dashboard_assets.index_dot_html
  | "/main.bc.js" ->
    respond
      ~content_type:"application/javascript"
      Dashboard_assets.main_dot_bc_dot_js
  | path ->
    Cohttp_async.Server.respond_string
      ~status:`Not_found
      [%string "no such resource: %{path}"]
;;
