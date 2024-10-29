(lang dune 3.6)
(version 0.4.1)
(name theora)
(source (github savonet/ocaml-theora))
(license GPL-2.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name theora)
 (synopsis "Bindings to libtheora")
 (depends
  conf-libtheora
  conf-pkg-config
  (dune (>= 2.0))
  dune-configurator
  (ogg (>= 0.7.0)))
)
