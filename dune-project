(lang dune 3.9)

(name ocasm6502)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Christophe Roux")

(maintainers "Christophe Roux")

(license MIT)

(documentation https://github.com/gloppasglop/6502-assembler/README.md)

(package
 (name ocasm6502)
 (synopsis "A 6502 Assembler")
 (description "A 6502 Assembler")
 (depends ocaml dune)
 (tags
  (topics "6502" assembler)))

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
