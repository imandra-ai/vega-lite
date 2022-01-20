(lang dune 2.0)
(generate_opam_files true)
(name vega-lite)

(package
  (name vega-lite)
  (synopsis "Generate vega-lite plots")
  (depends
    (ocaml (>= "4.08"))
    (yojson (>= "1.6"))))
