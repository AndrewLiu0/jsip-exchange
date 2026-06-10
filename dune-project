(lang dune 3.0)

(generate_opam_files true)

;; CR-soon abauer: update homepage and bug_reports to point to the right github page

(maintainers "JSIP Organizers")

(authors "JSIP Organizers")

(homepage "https://example.com")

(bug_reports "https://example.com/issues")

(license MIT)

(package
 (name jsip-exchange)
 (synopsis "An project building a financial exchange in OCaml")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  ocaml-lsp-server
  ocamlformat
  (odoc :with-doc)
  async
  core
  core_bench
  ppx_jane))
