module V01 = struct
  module Syn = Shaper.V01

  (* [a, b, ..c] *)
  let list ?spread items : 'a Syn.syntax =
    match spread with
    | Some tl -> Syn.Form ("list", List.append items tl)
    | None -> Syn.Form ("list", items)

  (* {a = 1, ~b, ..c} *)
  let record ?spread items : 'a Syn.syntax =
    match spread with
    | Some tl -> Syn.Form ("record", List.append items tl)
    | None -> Syn.Form ("record", items)

  (* (a, b, c) *)
  let tuple items : 'a Syn.syntax = Syn.Form ("tuple", items)

  (* {a, b, c} *)
  let array items : 'a Syn.syntax = Syn.Form ("array", items)

  (* { a; b; c } *)
  let block items : 'a Syn.syntax = Syn.Form ("block", items)

  (* ..a *)
  let spread x : 'a Syn.syntax = Syn.Form ("..", [ x ])

  (* a -> b *)
  let arrow a b : 'a Syn.syntax = Syn.Form ("->", [ a; b ])

  (* a : b *)
  let constraint_ a b : 'a Syn.syntax = Syn.Form (":", [ a; b ])

  (* a | b *)
  let alt a b : 'a Syn.syntax = Syn.Form ("|", [ a; b ])

  (* a.b *)
  let field a b : 'a Syn.syntax = Syn.Form (".", [ a; b ])

  (* a = b *)
  let binding a b : 'a Syn.syntax = Syn.Form ("=", [ a; b ])

  (* fn a b -> c *)
  let fn args body : 'a Syn.syntax = arrow (Syn.Form ("fn", args)) body

  (* open M *)
  let open_ mexp : 'a Syn.syntax = Syn.Form ("open", [ mexp ])

  (* f a b *)
  let apply f args : 'a Syn.syntax = Syn.Call (f, args)

  (* module Mb *)
  let module_ mb : 'a Syn.syntax = Syn.Form ("module", [ mb ])
end

module V03 = struct
  module Syn = Shaper.V03

  let lower = Syn.lower
  let upper = Syn.upper
  let string = Syn.string
  let int = Syn.int
  let float = Syn.float
  let char = Syn.char

  (* f a b *)
  let apply f args = Syn.seq (f :: args)

  (* X a *)
  let constr upper_id args = Syn.seq (Syn.upper upper_id :: args)

  (* (), (a, b, c) *)
  let tuple items = Syn.parens (Syn.seq_comma items)

  (* {}, {a}, {a, b, c} *)
  let array items = Syn.braces (Syn.seq_comma items)

  (* {a;} {a; b; c} *)
  let block items = Syn.braces (Syn.seq_semi items)

  (* [a, b, ..c] *)
  let list ?spread items =
    match spread with
    | Some tl -> Syn.brackets (Syn.seq_comma (List.append items tl))
    | None -> Syn.brackets (Syn.seq_comma items)

  (* {a = 1, ~b, ..c} *)
  let record ?spread items =
    match spread with
    | Some tl -> Syn.Form ("record", List.append items tl)
    | None -> Syn.Form ("record", items)

  (* ..a *)
  let spread x = Syn.Form ("..", [ x ])

  (* a | b | ... *)
  let alt items = Syn.Form ("|", items)

  (* a.b *)
  let dot a b = Syn.Form (".", [ a; b ])

  (* a = b *)
  let binding a b = Syn.Form ("=", [ a; b ])

  (* a -> b *)
  let arrow a b = Syn.Form ("->", [ a; b ])

  (* fn ... -> ... *)
  (* fn { ... } *)
  let fn form = Syn.form "fn" [ form ]

  (* a : b *)
  let constraint_ a b = Syn.Form (":", [ a; b ])

  (* f a b *)
  let apply f args = Syn.seq (f :: args)

  (* open M *)
  (* open { ... } *)
  let open_ mexp = Syn.form "open" [ mexp ]

  (* let a = 1; ... *)
  (* let a = 1, b = 2; ... *)
  let let_ vbl = Syn.form "let" [ vbl ]

  (* if a then b else c *)
  let if_then_else cond then_syn else_syn =
    Syn.form "if" [ cond; then_syn; else_syn ]

  (* match a { ... } *)
  let match_ arg = Syn.form "match" [ arg ]

  (* val a = 1 *)
  (* val a = 1, b = 2 *)
  let val_ vbl = Syn.form "val" [ vbl ]

  (* module M = ... *)
  let module_ mb = Syn.form "module" [ mb ]

  (* `a` *)
  let quote x = Syn.Scope ("`", x, "`")

  (* $a *)
  let antiquote x = Syn.form "$" [ x ]

  (* Meta representation of syntax. *)
  let rec meta (syn : Syn.syntax) =
    match syn with
    | Ident (Upper id) -> constr "Ident" [ constr "Upper" [ string id ] ]
    | Ident (Lower id) -> constr "Ident" [ constr "Lower" [ string id ] ]
    | Const (Int x) -> constr "Const" [ constr "Int" [ int x ] ]
    | Const (Float x) -> constr "Const" [ constr "Float" [ float x ] ]
    | Const (Char x) -> constr "Const" [ constr "Char" [ char x ] ]
    | Const (String x) -> constr "Const" [ constr "String" [ string x ] ]
    | Sym x -> constr "Sym" [ string x ]
    | Seq (None, items) ->
      constr "Seq" [ constr "None" []; list (List.map meta items) ]
    | Seq (Some sep, items) ->
      constr "Seq" [ constr "Some" [ string sep ]; list (List.map meta items) ]
    | Form ("$", [ code ]) -> code
    | Form (kwd, items) ->
      constr "Form" [ string kwd; list (List.map meta items) ]
    | Scope (l, x, r) -> constr "Scope" [ string l; meta x; string r ]
end
