open Prelude
module C = Fold_ast.Cons

(* Meta representation of syntax. *)
let rec quote (syn : Shaper.syntax) =
  let open C in
  let construct s = C.construct (Longident.Lident s) in
  match syn with
  | Ident (Upper id) -> construct "Ident" [ construct "Upper" [ string id ] ]
  | Ident (Lower id) -> construct "Ident" [ construct "Lower" [ string id ] ]
  | Const (Int x) -> construct "Const" [ construct "Int" [ int x ] ]
  | Const (Float x) -> construct "Const" [ construct "Float" [ float x ] ]
  | Const (Char x) -> construct "Const" [ construct "Char" [ char x ] ]
  | Const (String x) -> construct "Const" [ construct "String" [ string x ] ]
  | Sym x -> construct "Sym" [ string x ]
  | Seq (None, items) ->
    construct "Seq" [ construct "None" []; list (List.map quote items) ]
  | Seq (Some sep, items) ->
    construct "Seq"
      [ construct "Some" [ string sep ]; list (List.map quote items) ]
  | Scope (l, x, r) -> construct "Scope" [ string l; quote x; string r ]
  | Shape ("unquote", [ code ]) -> code
  | Shape (kwd, items) ->
    construct "Shape" [ string kwd; list (List.map quote items) ]
