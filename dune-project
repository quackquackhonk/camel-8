(lang dune 3.17)

(name camel8)

(generate_opam_files true)
(opam_file_location inside_opam_directory)

(source
 (github quackquackhonk/camel-8))

(authors "Sahana Tankala <sahanatankala@gmail.com>")

(maintainers "Sahana Tankala <sahanatankala@gmail.com>")

(package
 (name camel8)
 (synopsis "A Chip-8 interpreter in OCaml")
 (depends ocaml unix lwt lambda-term (alcotest :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
