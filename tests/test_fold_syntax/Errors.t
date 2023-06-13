
#  $ echo "yo"
#  1
#
#  $ fl -i fl -o ml - <<< '2 + 2'
#
#  $ fl -i fl -o ml - << END
#  > 2 + 2;
#  > let x = 1;
#  > END
#
#  $ fl -i fl -o ml - << END
#  > let 1
#  > END
#
#  $ fl -i fl -o ml - << END
#  > let = 1
#  > END
#
#  $ fl -i fl -o ml - << END
#  > let 1 + 1
#  > END
#
#  $ fl -i fl -o ml - << END
#  > match {}
#  > END
#
#  $ fl -i fl -o ml - << END
#  > e!
#  > END


Module id:

#   $ fl -i fl -o ml - <<< '{ r.M }'
#   syntax error File "-", line 1, characters 6-7:
#   fold-fmt: internal error, uncaught exception:
#             Failure("invalid field: M")
#             Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
#             Called from Pratt.parse_infix in file "src/pratt/Pratt.ml", line 149, characters 17-30
#             Called from Pratt.prefix_scope.rule in file "src/pratt/Pratt.ml", line 277, characters 15-24
#             Called from Pratt.parse in file "src/pratt/Pratt.ml", line 155, characters 14-30
#             Called from Pratt.run in file "src/pratt/Pratt.ml", line 159, characters 8-17
#             Called from Fold__Fold_parser.parse in file "src/fold/Fold_parser.ml", line 273, characters 6-21
#             Re-raised at Fold__Fold_parser.parse in file "src/fold/Fold_parser.ml", line 277, characters 4-13
#             Called from Dune__exe__Main.run in file "src/fold_fmt/Main.ml", line 10, characters 13-65
#             Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
#             Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
#   [125]
#   $ fl -i fl -o ml - <<< '{ x1.(M) }'
#   fold-fmt: internal error, uncaught exception:
#             Failure("invalid field label: M")
#             Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
#             Called from Fold__Fold_eval.Eval.Structure_item.eval_eval in file "src/fold/Fold_eval.ml", line 1227, characters 45-65
#             Called from Fold__Fold_eval.Eval.structure in file "src/fold/Fold_eval.ml", line 1465, characters 13-35
#             Called from Dune__exe__Main.run in file "src/fold_fmt/Main.ml", line 11, characters 13-39
#             Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
#             Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
#   [125]

#   $ fl -i fl -o ml - <<< '{ r.M.x }'
#   syntax error File "-", line 1, characters 5-6:
#   fold-fmt: internal error, uncaught exception:
#             Failure("invalid field: M")
#             Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
#             Called from Pratt.parse_infix in file "src/pratt/Pratt.ml", line 149, characters 17-30
#             Called from Pratt.prefix_scope.rule in file "src/pratt/Pratt.ml", line 277, characters 15-24
#             Called from Pratt.parse in file "src/pratt/Pratt.ml", line 155, characters 14-30
#             Called from Pratt.run in file "src/pratt/Pratt.ml", line 159, characters 8-17
#             Called from Fold__Fold_parser.parse in file "src/fold/Fold_parser.ml", line 273, characters 6-21
#             Re-raised at Fold__Fold_parser.parse in file "src/fold/Fold_parser.ml", line 277, characters 4-13
#             Called from Dune__exe__Main.run in file "src/fold_fmt/Main.ml", line 10, characters 13-65
#             Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
#             Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
#   [125]

#   $ fl -i fl -o ml - <<< '{ x1.(M2.x2.x3) }'
#   >>> (Shape ("field",
#         [(Shape ("ident", [(Ident M2); (Ident x2)])); (Ident x3)]))
#   fold-fmt: internal error, uncaught exception:
#             Failure("invalid ident")
#             Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
#             Called from Fold__Fold_eval.Eval.Expression.eval in file "src/fold/Fold_eval.ml", line 109, characters 18-35
#             Called from Fold__Fold_eval.Eval.Structure_item.eval_eval in file "src/fold/Fold_eval.ml", line 1227, characters 45-65
#             Called from Fold__Fold_eval.Eval.structure in file "src/fold/Fold_eval.ml", line 1465, characters 13-35
#             Called from Dune__exe__Main.run in file "src/fold_fmt/Main.ml", line 11, characters 13-39
#             Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
#             Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
#   [125]
