(lang dune 3.6)
(version 0.4.2)
(name theora)
(source (github savonet/ocaml-theora))
(license GPL-2.0-only)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name theora)
 (synopsis "Bindings to libtheora")
 (depends
  conf-libtheora
  conf-pkg-config
  dune
  dune-configurator
  (ogg (>= 0.8.0)))
)
