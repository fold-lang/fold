(lang dune 3.6)

(name fold)

(implicit_transitive_deps false)

(using menhir 2.1)

(cram enable)

(dialect
 (name fold)
 (implementation
  (extension fl)
  (preprocess
   (run fl -i fl -o bin %{input-file})))
 (interface
  (extension fli)
  (preprocess
   (run fl %{input-file}))))
