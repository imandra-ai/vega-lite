(lang dune 2.0)
(generate_opam_files true)
(license MIT)
(homepage "https://github.com/AestheticIntegration/vega-lite")
(authors "simon@imandra.ai")
(maintainers "simon@imandra.ai")
(source (github AestheticIntegration/vega-lite.git))
(name vega-lite)

(package
  (name vega-lite)
  (synopsis "Generate vega-lite plots")
  (depends
    (ocaml (>= "4.08"))
    (yojson (>= "1.6"))))
