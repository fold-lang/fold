(lang dune 3.0)

(name fold)

(implicit_transitive_deps false)

(using menhir 2.1)

(cram enable)

(dialect
 (name fold)
 (implementation
  (extension fl)
  (preprocess
   (run fold-fmt -i fl -o ml %{input-file})))
 (interface
  (extension fli)
  (preprocess
   (run fold-fmt %{input-file}))))
