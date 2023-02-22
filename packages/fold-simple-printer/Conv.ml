let todo = Syntax.ident "TODO"

let rec conv_exp (exp : Parsetree.expression) =
  match exp.pexp_desc with
  | Pexp_ident { txt = ident; _ } -> Syntax.Ident ident
  | Pexp_constant constant -> Syntax.Constant constant
  | Pexp_let (rec_flag, vbl, body) -> let_ rec_flag vbl body
  | Pexp_apply (f_exp, args) -> apply f_exp args
  | Pexp_tuple items -> tuple items
  | Pexp_record (bindings, r0) -> record bindings r0
  | Pexp_sequence (exp_1, exp_2) -> sequence exp_1 exp_2
  | _ -> todo

and conv_pat (pat : Parsetree.pattern) =
  match pat.ppat_desc with
  | Ppat_var { txt = var; _ } -> Syntax.ident var
  | _ -> todo

and conv_vb (vb : Parsetree.value_binding) =
  let pat' = conv_pat vb.pvb_pat in
  let exp' = conv_exp vb.pvb_expr in
  Syntax.Binding (pat', exp')

and let_ ?loc:_ ?attrs:_ rec_flag vbl body =
  let rec flatten (exp : Parsetree.expression) acc =
    match exp.pexp_desc with
    | Pexp_let (rec_flag, vbl, body) ->
      let kwd =
        match rec_flag with
        | Recursive -> "let-rec"
        | Nonrecursive -> "let"
      in
      let vbl' = Syntax.Seq (List.map conv_vb vbl) in
      let form = Syntax.Form ([ kwd ], [ vbl' ]) in
      flatten body (form :: acc)
    | Pexp_sequence (exp_1, exp_2) -> flatten exp_2 (conv_exp exp_1 :: acc)
    | _ -> List.rev (conv_exp exp :: acc)
  in
  let kwd =
    match rec_flag with
    | Recursive -> "let-rec"
    | Nonrecursive -> "let"
  in
  let vbl' = Syntax.Seq (List.map conv_vb vbl) in
  let body' = flatten body [] in
  let form = Syntax.Form ([ kwd ], [ vbl' ]) in
  Syntax.Block (form :: body')

and apply ?loc:_ ?attrs:_ f_exp args =
  let f_exp' = conv_exp f_exp in
  let args' = List.map conv_arg args in
  Syntax.Apply (f_exp', args')

and conv_arg (arg_label, exp) =
  match arg_label with
  | Labelled l -> Syntax.Form ([ "~"; l ], [ conv_exp exp ])
  | Optional l -> Syntax.Form ([ "?"; l ], [ conv_exp exp ])
  | Nolabel -> conv_exp exp

and tuple ?loc:_ ?attrs:_ items =
  Syntax.Block [ Syntax.Seq (List.map conv_exp items) ]

and sequence ?loc:_ ?attrs:_ exp0_1 exp0_2 =
  let rec flatten (exp : Parsetree.expression) acc =
    match exp.pexp_desc with
    | Pexp_sequence (exp_1, exp_2) -> flatten exp_2 (conv_exp exp_1 :: acc)
    | _ -> List.rev (conv_exp exp :: acc)
  in
  Syntax.Block (conv_exp exp0_1 :: flatten exp0_2 [])

and record ?loc:_ ?attrs:_ bindings r0 =
  let bindings' =
    List.map
      (fun ({ Location.txt = lid; _ }, v) ->
        Syntax.Binding (Syntax.Ident lid, conv_exp v))
      bindings
  in
  match r0 with
  | Some r0 ->
    Syntax.Block
      [ Syntax.Seq (Syntax.Form ([ "..." ], [ conv_exp r0 ]) :: bindings') ]
  | None -> Syntax.Block bindings'
