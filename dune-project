(lang dune 2.7)
(generate_opam_files true)

(source (github rbardou/typath))
(license MIT)
(authors "Romain Bardou")
(maintainers "Romain Bardou")

(package
 (name typath)
 (synopsis "Typed file system path representation")
 (description "Parsing, string representation, concatenation and other tools for typed path manipulation.")
 (depends (ocaml (>= 4.08))))
