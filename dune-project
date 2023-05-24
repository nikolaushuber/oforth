(lang dune 3.7)

(name oforth)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Nikolaus Huber")

(maintainers "Nikolaus Huber")

(license EUPL)

(documentation https://url/to/documentation)

(package
 (name oforth)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (cmdliner (>= 1.2.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
