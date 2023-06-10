open Prelude

module Eval (E : Fold_ast_builder.S) : sig
  val expression : fl -> E.expression
  val pattern : fl -> E.pattern
  val core_type : fl -> E.core_type
  val value_binding : fl -> E.value_binding
  val module_binding : fl -> E.module_binding
  val structure_item : fl -> E.structure_item
  val structure : fl -> E.structure_item list
end = struct
  open struct
    let pexp_unit = E.pexp_construct (with_noloc (Ident.Lident "()")) None
    let pexp_nil = E.pexp_construct (with_noloc (Ident.Lident "[]")) None

    let pexp_cons ~loc x xs =
      E.pexp_construct ~loc
        (with_noloc (Ident.Lident "::"))
        (Some (E.pexp_tuple ~loc [ x; xs ]))

    let ppat_nil = E.ppat_construct (with_noloc (Ident.Lident "[]")) None

    let ppat_cons ~loc x xs =
      (* [TODO] tvars *)
      E.ppat_construct ~loc
        (with_noloc (Ident.Lident "::"))
        (Some (E.ppat_tuple ~loc [ x; xs ]))
  end

  let conv_const (const : Shaper.const) =
    let module C = Ppxlib.Ast_helper.Const in
    match const with
    | Int x -> C.int x
    | Float x -> C.float (string_of_float x)
    | String x -> C.string x
    | Char x -> C.char x

  let eval_expident (fl : fl) : Ident.t with_loc =
    match fl with
    | Ident (Upper id | Lower id) -> with_noloc (Ident.Lident id)
    | Shape (loc, "ident", [ Ident (Upper id | Lower id) ]) ->
      with_loc loc (Ident.Lident id)
    | Shape (loc, "ident", Ident (Upper hd) :: rest) ->
      let ident =
        List.fold_left
          (fun path (fl : fl) ->
            match fl with
            | Ident (Upper id | Lower id) -> Ident.Ldot (path, id)
            | _ -> failwith "invalid longident component"
          )
          (Ident.Lident hd) rest
      in
      with_loc loc ident
    | Shape (_, "ident", []) -> failwith "invalid ident: empty parts"
    | _ ->
      Fmt.epr ">>> %a@." Shaper.dump fl;
      failwith "invalid ident"

  let rec quasiquote (syn : Shaper.syntax) =
    let open Fold_ast.Cons in
    let construct s = Fold_ast.Cons.construct (Ident.Lident s) in
    match syn with
    | Scope ("(", x, ")") -> quasiquote x
    | Ident (Upper id) -> construct "Ident" [ construct "Upper" [ string id ] ]
    | Ident (Lower id) -> construct "Ident" [ construct "Lower" [ string id ] ]
    | Const (Int x) -> construct "Const" [ construct "Int" [ int x ] ]
    | Const (Float x) -> construct "Const" [ construct "Float" [ float x ] ]
    | Const (Char x) -> construct "Const" [ construct "Char" [ char x ] ]
    | Const (String x) -> construct "Const" [ construct "String" [ string x ] ]
    | Sym x -> construct "Sym" [ string x ]
    | Seq items ->
      construct "Seq" [ construct "None" []; list (List.map quasiquote items) ]
    | Scope (l, x, r) -> construct "Scope" [ string l; quasiquote x; string r ]
    | Shape (_loc, "unquote", [ code ]) -> code
    | Shape (loc, kwd, items) ->
      let noloc = Shaper.shape ~loc "ident" [ upper "Shaper"; lower "noloc" ] in
      construct "Shape" [ noloc; string kwd; list (List.map quasiquote items) ]

  module rec Expression : sig
    val eval : fl -> E.expression
    val eval_fn : ?loc:loc -> fl list -> fl -> E.expression
  end = struct
    let rec eval (fl : fl) =
      match fl with
      (* --- ident --- *)
      | Ident (Lower "true") | Ident (Lower "false") ->
        failwith "True and False must be start with a capital letter"
      (* x *)
      | Ident (Lower x) -> E.pexp_ident ~loc:noloc (with_noloc (Ident.Lident x))
      (* TODO + *)
      | Sym x -> E.pexp_ident ~loc:noloc (with_noloc (Ident.Lident x))
      (* M.a.b *)
      | Shape (loc, "ident", _) ->
        let ident = eval_expident fl in
        E.pexp_ident ~loc ident
      (* --- field --- *)
      (* err: !(field ) *)
      | Shape (_loc, "field", []) ->
        Fmt.failwith "invalid field syntax: empty shape"
      (* err: !(field expr) *)
      | Shape (_loc, "field", [ _ ]) ->
        Fmt.failwith "invalid field syntax: missing field"
      (* err: r.M *)
      | Shape (_loc, "field", [ _; Ident (Upper id) ]) ->
        Fmt.failwith "invalid field label: %s" id
      (* r1.r2.r3.field *)
      | Shape (loc, "field", expr0 :: id0 :: ids) ->
        let expr = eval expr0 in
        let id0 = eval_expident id0 in
        let expr0 = E.pexp_field ~loc expr id0 in
        List.fold_left
          (fun expr id ->
            let id = eval_expident id in
            E.pexp_field ~loc expr id
          )
          expr0 ids
      (* --- let_in_unit --- *)
      | Shape (loc, "let", [ Shape (_loc, ",", vbl) ]) ->
        eval_let_in_unit ~loc Asttypes.Nonrecursive vbl
      | Shape (loc, "let", [ Scope ("(", Shape (_, ",", vbl), ")") ]) ->
        eval_let_in_unit ~loc Asttypes.Nonrecursive vbl
      | Shape (loc, "let", [ vb ]) ->
        eval_let_in_unit ~loc Asttypes.Nonrecursive [ vb ]
      (* --- rec_in_unit --- *)
      | Shape (loc, "rec", [ Shape (_loc, ",", vbl) ]) ->
        eval_let_in_unit ~loc Asttypes.Recursive vbl
      | Shape (loc, "rec", [ Scope ("(", Shape (_, ",", vbl), ")") ]) ->
        eval_let_in_unit ~loc Asttypes.Recursive vbl
      | Shape (loc, "rec", [ vb ]) ->
        eval_let_in_unit ~loc Asttypes.Recursive [ vb ]
      (* --- const --- *)
      | Const const -> E.pexp_constant ~loc:noloc (conv_const const)
      (* --- lambda --- *)
      (* "fn" args... "->" body *)
      | Shape (loc, "->", [ Shape (_, "fn", args); body ]) ->
        eval_fn ~loc args body
      (* "fn" arg "->" body *)
      | Shape (loc, "->", [ Shape (_, "fn", [ arg; body ]) ]) ->
        eval_fn ~loc [ arg ] body
      (* "fn" "{" arg "->" body "}" *)
      | Shape (loc, "fn", [ Scope ("{", Shape (_, "->", [ arg; body ]), "}") ])
        -> eval_fn ~loc [ arg ] body
      (* --- fn_match --- *)
      | Shape
          ( loc
          , "fn"
          , [ Scope
                ( "{"
                , Shape (_, ",", (Shape (_, "->", [ _; _ ]) :: _ as cases))
                , "}"
                )
            ]
          ) -> eval_fn_match ~loc cases
      (* -- record -- *)
      (* `{ ~l, ...}` or `{ l = e, ... }` *)
      | Scope
          ("{", Shape (_, ",", (Shape (_, ("~" | "="), _) :: _ as fields)), "}")
        -> eval_record fields
      (* `{ r & ~l, ...}` or `{ r & l = e, ... }` *)
      | Scope
          ( "{"
          , Shape
              ( _
              , "&"
              , [ r
                ; Shape (_, ",", (Shape (_, ("~" | "="), _) :: _ as fields))
                ]
              )
          , "}"
          ) -> eval_record ~spread:r fields
      (* `{ ~l }` or `{ l = e }` *)
      | Scope ("{", (Shape (_, ("~" | "="), _) as field), "}") ->
        eval_record [ field ]
      (* `{ r & ~l }` or `{ r & l = e }` *)
      | Scope
          ("{", Shape (_, "&", [ r; (Shape (_, ("~" | "="), _) as field) ]), "}")
        -> eval_record ~spread:r [ field ]
      (* --- if_then_else --- *)
      | Shape (loc, "if", [ a; b; c ]) -> eval_if_then_else ~loc a b (Some c)
      (* --- if_then --- *)
      | Shape (loc, "if", [ a; b ]) -> eval_if_then_else ~loc a b None
      | Shape (loc, "if", [ Scope ("{", Shape (_, ",", cases), "}") ]) ->
        eval_if_cases ~loc cases
      | Shape (loc, "if", [ Scope ("{", c, "}") ]) -> eval_if_cases ~loc [ c ]
      (* --- match --- *)
      | Shape (loc, "match", [ a; Scope ("{", Shape (_, ",", b), "}") ]) ->
        eval_match ~loc a b
      | Shape (loc, "match", [ _a; Scope ("{", Seq [], "}") ]) ->
        (* XXX: generates an error attribute for the syntax error *)
        let attr =
          E.attribute ~loc ~name:(with_noloc "ocaml.alert")
            ~payload:
              (E.pstr
                 [ E.pstr_eval ~loc
                     (E.pexp_apply ~loc
                        (E.pexp_ident ~loc (with_loc loc (Ident.Lident "error")))
                        [ ( Asttypes.Nolabel
                          , E.pexp_constant ~loc
                              (Ppxlib.Ast_helper.Const.string "invalid match")
                          )
                        ]
                     )
                     []
                 ]
              )
        in
        E.pexp_with_attributes [ attr ] (pexp_unit ~loc)
      (* match { _ } *)
      | Shape (loc, "match", [ a; Scope ("{", single_case, "}") ]) ->
        eval_match ~loc a [ single_case ]
      | Shape (loc, "match", _) ->
        Fmt.failwith "invalid match syntax: %a" Location.print_loc
          (Obj.magic loc)
      (* -- try --- *)
      (* try _ { _, ... } *)
      | Shape (loc, "try", [ a; Scope ("{", Shape (_, ",", b), "}") ]) ->
        eval_try ~loc a b
      (* try _ { _ } *)
      | Shape (loc, "try", [ a; Scope ("{", b, "}") ]) -> eval_try ~loc a [ b ]
      (* --- construct --- *)
      (* TODO: explicit arity *)
      | Ident (Upper "True") ->
        eval_construct ~loc:noloc (Ident.Lident "true") []
      | Ident (Upper "False") ->
        eval_construct ~loc:noloc (Ident.Lident "false") []
      | Ident (Upper id) -> eval_construct ~loc:noloc (Ident.Lident id) []
      (* () *)
      | Scope ("(", Seq [], ")") ->
        eval_construct ~loc:noloc (Ident.Lident "()") []
      (* {} *)
      | Scope ("{", Seq [], "}") ->
        eval_construct ~loc:noloc (Ident.Lident "()") []
      | Seq (Ident (Upper c) :: args) ->
        eval_construct ~loc:noloc (Ident.Lident c) args
      (* --- polyvar --- *)
      (* TODO: explicit arity *)
      | Seq (Shape (loc, "#", [ Ident (Upper id) ]) :: args) ->
        eval_variant ~loc id args
      | Shape (loc, "#", [ Ident (Upper id) ]) -> eval_variant ~loc id []
      | Shape (_loc, "#", _) -> failwith "invalid polyvar syntax"
      (* --- apply or macro --- *)
      | Seq (Sym "-" :: [ x ]) -> eval_apply (Shaper.sym "~-") [ x ]
      | Seq (Sym "+" :: [ x ]) -> eval_apply (Shaper.sym "~+") [ x ]
      | Seq (f :: args) -> eval_apply f args
      (* --- a; b; b --- *)
      | Shape (loc, ";", xs) -> eval_block ~loc xs
      (* Err: a, b *)
      | Shape (_loc, ",", _xs) ->
        Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl;
        failwith "err: a, b"
      (* --- () --- *)
      | Scope ("(", Shape (_, ",", []), ")") ->
        eval_construct ~loc:noloc (Ident.Lident "()") []
      | Scope ("(", Shape (_, ",", items), ")") -> eval_tuple ~loc:noloc items
      (* --- group --- *)
      | Scope ("(", fl, ")") -> eval fl
      | Scope ("{", x, "}") -> eval x
      (* -- e : t -- *)
      | Shape (_loc, ":", [ e; t ]) ->
        let loc = Ppxlib.Location.none in
        let e = eval e in
        let t = Core_type.eval t in
        E.pexp_constraint ~loc e t
      (* -- while -- *)
      | Shape (loc, "while", [ a; b ]) -> eval_while ~loc a b
      (* --- for --- *)
      | Shape (loc, "for", [ Scope ("(", binding, ")"); body ]) ->
        eval_for ~loc binding body
      (* --- list --- *)
      (* [a, b & tl] *)
      | Scope ("[", Shape (loc, "&", [ Shape (_, ",", items); tl ]), "]") ->
        eval_list ~loc ?spread:(Some tl) items
      (* [a & tl] *)
      | Scope ("[", Shape (_loc, "&", [ a; tl ]), "]") ->
        eval_list ~loc:noloc ?spread:(Some tl) [ a ]
      (* [a, b, c] --- *)
      | Scope ("[", Shape (loc, ",", items), "]") ->
        eval_list ~loc ?spread:None items
      (* [] *)
      | Scope ("[", Seq [], "]") -> eval_list ~loc:noloc ?spread:None []
      (* [a] *)
      | Scope ("[", item, "]") -> eval_list ~loc:noloc ?spread:None [ item ]
      (* --- quote --- *)
      | Shape (_loc, "quote", [ x ]) -> eval (quasiquote x)
      | Shape (_loc, "quote", xs) -> eval (quasiquote (Shaper.seq xs))
      (* --- macro --- *)
      | Shape (loc, kwd, args) -> begin
        match Fold_macros.getmacro kwd with
        | Some macro -> eval (macro args)
        | None ->
          (* Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl; *)
          (* Fmt.failwith "unknown macro %S" kwd *)
          let id = with_loc loc ("fl." ^ kwd) in
          let arg =
            match args with
            | [ x ] -> x
            | _ -> Shaper.seq args
          in
          let arg = Shaper.String (Fmt.str "%a" Shaper.pp_sexp arg) in
          let arg = E.pexp_constant ~loc (conv_const arg) in
          let item = E.pstr_eval ~loc arg [] in
          let payload = E.pstr [ item ] in
          E.pexp_extension ~loc (id, payload)
      end
      (* Err: unknown *)
      | _ ->
        Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl;
        assert false

    and eval_apply_arg (fl : fl) =
      match fl with
      (* ~a | ~(a) *)
      | Shape (loc, "~", [ Scope ("(", Ident (Lower label), ")") ])
      | Shape (loc, "~", [ Ident (Lower label) ]) ->
        ( Asttypes.Labelled label
        , E.pexp_ident ~loc (with_loc loc (Ident.Lident label))
        )
      (* ~a? | ~(a?) *)
      | Shape
          ( loc
          , "~"
          , [ Scope ("(", Shape (_, "?", [ Ident (Lower label) ]), ")") ]
          )
      | Shape (loc, "~?", [ Ident (Lower label) ]) ->
        ( Asttypes.Optional label
        , E.pexp_ident ~loc (with_loc loc (Ident.Lident label))
        )
      (* ~(a = _) *)
      | Shape
          ( _loc
          , "~"
          , [ Scope ("(", Shape (_, "=", [ Ident (Lower label); arg_val ]), ")")
            ]
          ) -> (Asttypes.Labelled label, eval arg_val)
      (* ~(a : _) *)
      | Shape
          ( loc
          , "~"
          , [ Scope
                ("(", Shape (cons_loc, ":", [ Ident (Lower label); typ ]), ")")
            ]
          ) ->
        ( Asttypes.Labelled label
        , E.pexp_constraint ~loc:cons_loc
            (E.pexp_ident ~loc:cons_loc (with_loc loc (Ident.Lident label)))
            (Core_type.eval typ)
        )
      (* ~(_ : _ = _) *)
      | Shape
          ( loc
          , "~"
          , [ Scope
                ( "("
                , Shape
                    ( _eq_loc
                    , "="
                    , [ Shape (_cons_loc, ":", [ Ident (Lower label); typ ])
                      ; arg_val
                      ]
                    )
                , ")"
                )
            ]
          ) ->
        ( Asttypes.Labelled label
        , E.pexp_constraint ~loc (eval arg_val) (Core_type.eval typ)
        )
      (* ~(_? = _) *)
      | Shape
          ( _loc
          , "~"
          , [ Scope
                ( "("
                , Shape
                    ( _
                    , "="
                    , [ Shape (_, "?", [ Ident (Lower label) ]); arg_val ]
                    )
                , ")"
                )
            ]
          ) -> (Asttypes.Optional label, eval arg_val)
      (* ~(_? : _ = _) *)
      | Shape
          ( loc
          , "~"
          , [ Scope
                ( "("
                , Shape
                    ( _eq_loc
                    , "="
                    , [ Shape
                          ( _cons_loc
                          , ":"
                          , [ Shape (_, "?", [ Ident (Lower label) ]); typ ]
                          )
                      ; arg_val
                      ]
                    )
                , ")"
                )
            ]
          ) ->
        ( Asttypes.Optional label
        , E.pexp_constraint ~loc (eval arg_val) (Core_type.eval typ)
        )
      (* _ *)
      | _ -> (Asttypes.Nolabel, eval fl)

    and eval_apply f_fl args_fl =
      let f_ml = eval f_fl in
      let args_ml = List.map eval_apply_arg args_fl in
      E.pexp_apply ~loc:noloc f_ml args_ml

    and eval_apply' a b =
      let a = eval a in
      E.pexp_apply ~loc:noloc a [ eval_apply_arg b ]

    and eval_variant ~loc label args_fl =
      match args_fl with
      | [] -> E.pexp_variant ~loc label None
      | [ arg_fl ] ->
        let arg_ml = eval arg_fl in
        E.pexp_variant ~loc label (Some arg_ml)
      | _ ->
        (* [TODO] Add explicity arity ext like Reason. *)
        let arg_ml = E.pexp_tuple ~loc (List.map eval args_fl) in
        E.pexp_variant ~loc label (Some arg_ml)

    and eval_construct ~loc id args_fl =
      match args_fl with
      | [] -> E.pexp_construct ~loc (with_noloc id) None
      | [ arg_fl ] ->
        let arg_ml = eval arg_fl in
        E.pexp_construct ~loc (with_noloc id) (Some arg_ml)
      | _ ->
        (* [TODO] Add explicity arity ext like Reason. *)
        let arg_ml = E.pexp_tuple ~loc (List.map eval args_fl) in
        E.pexp_construct ~loc (with_noloc id) (Some arg_ml)

    and eval_fn ?loc:_ (args_fl : fl list) (body_fl : fl) =
      let body_ml = eval body_fl in
      List.fold_left
        (fun acc (arg_fl : fl) ->
          match arg_fl with
          (* ~a | (~a) *)
          | Shape (loc, "~", [ Scope ("(", Ident (Lower label), ")") ])
          | Shape (loc, "~", [ Ident (Lower label) ]) ->
            let pat = E.ppat_var ~loc (with_loc loc label) in
            let default = None in
            let label = Asttypes.Labelled label in
            E.pexp_fun ~loc label default pat acc
          (* ~a? *)
          | Shape (loc, "~?", [ Ident (Lower label) ]) ->
            let pat = E.ppat_var ~loc (with_loc loc label) in
            let default = None in
            let label = Asttypes.Optional label in
            E.pexp_fun ~loc label default pat acc
          (* ~(_ = _) *)
          | Shape
              ( _
              , "~"
              , [ Scope
                    ( "("
                    , Shape (loc, "=", [ Ident (Lower label); default ])
                    , ")"
                    )
                ]
              ) ->
            let pat = E.ppat_var ~loc (with_loc loc label) in
            let default = Some (Expression.eval default) in
            let label = Asttypes.Optional label in
            E.pexp_fun ~loc label default pat acc
          (* ~(_ : _) *)
          | Shape
              ( loc
              , "~"
              , [ Scope
                    ( "("
                    , Shape (typ_loc, ":", [ Ident (Lower label); typ ])
                    , ")"
                    )
                ]
              ) ->
            let pat = E.ppat_var ~loc (with_loc loc label) in
            let typ = Core_type.eval typ in
            let pat = E.ppat_constraint ~loc:typ_loc pat typ in
            let default = None in
            let label = Asttypes.Labelled label in
            E.pexp_fun ~loc label default pat acc
          (* ~(_ as _) *)
          | Shape
              ( loc
              , "~"
              , [ Scope ("(", Shape (_, "as", [ Ident (Lower label); pat ]), ")")
                ]
              ) ->
            let pat = Pattern.eval pat in
            let default = None in
            let label = Asttypes.Optional label in
            E.pexp_fun ~loc label default pat acc
          (* ~(_ as _ = _) *)
          | Shape
              ( loc
              , "~"
              , [ Scope
                    ( "("
                    , Shape
                        ( _
                        , "="
                        , [ Shape (_, "as", [ Ident (Lower label); pat ])
                          ; default
                          ]
                        )
                    , ")"
                    )
                ]
              ) ->
            let pat = Pattern.eval pat in
            let default = Some (Expression.eval default) in
            let label = Asttypes.Optional label in
            E.pexp_fun ~loc label default pat acc
          (* ~(_? as _) *)
          | Shape
              ( loc
              , "~"
              , [ Scope
                    ( "("
                    , Shape
                        ( _as_loc
                        , "as"
                        , [ Shape (_opt_loc, "?", [ Ident (Lower label) ])
                          ; pat
                          ]
                        )
                    , ")"
                    )
                ]
              ) ->
            let pat = Pattern.eval pat in
            let default = None in
            let label = Asttypes.Optional label in
            E.pexp_fun ~loc label default pat acc
          (* _ *)
          | _ ->
            let pat = Pattern.eval arg_fl in
            let default = None in
            let label = Asttypes.Nolabel in
            E.pexp_fun ~loc:noloc label default pat acc
        )
        body_ml (List.rev args_fl)

    and eval_fn_match ~loc (cases_fl : fl list) =
      let cases = List.map eval_case cases_fl in
      E.pexp_function ~loc cases

    and eval_match ~loc exp_fl cases_fl =
      let exp_ml = eval exp_fl in
      let cases_ml = List.map eval_case cases_fl in
      E.pexp_match ~loc exp_ml cases_ml

    and eval_try ~loc exp_fl cases_fl =
      let exp_ml = eval exp_fl in
      let cases_ml = List.map eval_case cases_fl in
      E.pexp_try ~loc exp_ml cases_ml

    and eval_if_then_else ~loc a b c =
      let a = eval a in
      let b = eval b in
      let c = Option.map eval c in
      E.pexp_ifthenelse ~loc a b c

    and eval_if_cases ~loc (cs0 : fl list) =
      let rec loop (cs : fl list) =
        match cs with
        | [] -> None
        | Shape (_loc, "->", [ Ident (Lower "else"); else_body ]) :: _cs ->
          let else_body = eval else_body in
          Some else_body
        | Shape (_loc, "->", [ cond; body ]) :: cs ->
          let cond = eval cond in
          let body = eval body in
          let else_body = loop cs in
          Some (E.pexp_ifthenelse ~loc cond body else_body)
        | _ -> failwith "invalid case syntax in if"
      in
      match cs0 with
      | [] -> failwith "empty cases in if"
      | Shape (_loc, "->", [ Ident (Lower "else"); _body ]) :: _cs ->
        failwith "no condition cases apart from else in if"
      | Shape (_loc, "->", [ cond; body ]) :: cs ->
        let cond = eval cond in
        let body = eval body in
        let else_body = loop cs in
        E.pexp_ifthenelse ~loc cond body else_body
      | _ -> failwith "invalid if syntax"

    and eval_while ~loc a b =
      let a = eval a in
      let b = eval b in
      E.pexp_while ~loc a b

    and eval_record ?spread:r fields =
      let r = Option.map eval r in
      let fields =
        List.map
          (fun (field : fl) ->
            match field with
            | Shape (_, "=", [ Ident (Lower id); e ]) ->
              let l = with_noloc (Ident.Lident id) in
              let e = eval e in
              (l, e)
            | Shape (_, "~", [ Ident (Lower id) ]) ->
              let l = with_noloc (Ident.Lident id) in
              let e = E.pexp_ident ~loc:noloc l in
              (l, e)
            | _ -> failwith "invalid record field syntax"
          )
          fields
      in
      E.pexp_record ~loc:noloc fields r

    and eval_tuple ~loc items = E.pexp_tuple ~loc (List.map eval items)

    and eval_case (fl : fl) =
      match fl with
      | Shape (_loc, "->", [ Shape (_, "_if_", [ pat_fl; guard ]); exp_fl ]) ->
        let lhs = Pattern.eval pat_fl in
        let rhs = eval exp_fl in
        let guard = Some (eval guard) in
        E.case ~lhs ~guard ~rhs
      | Shape (_loc, "->", [ pat_fl; exp_fl ]) ->
        let lhs = Pattern.eval pat_fl in
        let rhs = eval exp_fl in
        E.case ~lhs ~guard:None ~rhs
      | _ ->
        Fmt.epr "%a@." Shaper.dump fl;
        assert false

    and eval_for ~loc (binding : fl) (body : fl) =
      match binding with
      | Shape (_loc, "=", [ p; Shape (_, "to", [ e1; e2 ]) ]) ->
        let p = Pattern.eval p in
        let e1 = eval e1 in
        let e2 = eval e2 in
        let body = eval body in
        E.pexp_for ~loc p e1 e2 Asttypes.Upto body
      | Shape (_loc, "=", [ p; Shape (_, "downto", [ e1; e2 ]) ]) ->
        let p = Pattern.eval p in
        let e1 = eval e1 in
        let e2 = eval e2 in
        let body = eval body in
        E.pexp_for ~loc p e1 e2 Asttypes.Downto body
      | _ -> failwith "ivalid for binding syntax"

    and eval_block ~loc (xs : fl list) =
      match xs with
      | [] -> pexp_unit ~loc
      | [ x ] -> eval x
      (* "let" ... "," ... *)
      | Shape (loc, "let", [ Scope ("(", Shape (_, ",", vbl), ")") ]) :: xs
      | Shape (loc, "let", [ Shape (_, ",", vbl) ]) :: xs ->
        let vbl_ml = List.map Value_binding.eval vbl in
        let body_ml = eval_block ~loc xs in
        E.pexp_let ~loc Asttypes.Nonrecursive vbl_ml body_ml
      (* let ... = _; ... *)
      | Shape
          ( loc
          , "let"
          , [ Shape
                ( _
                , "="
                , [ Seq
                      ((Ident (Lower ident) | Scope ("(", Sym ident, ")"))
                      :: args
                      )
                  ; body
                  ]
                )
            ]
          )
        :: xs ->
        let pat = E.ppat_var ~loc (with_loc loc ident) in
        let expr = Expression.eval_fn args body in
        let vbl_ml = [ E.value_binding ~loc ~pat ~expr ] in
        (* let vbl_ml = [ Value_binding.eval vb ] in *)
        let body_ml = eval_block ~loc xs in
        E.pexp_let ~loc Asttypes.Nonrecursive vbl_ml body_ml
      (* let _ *)
      | Shape (loc, "let", [ vb ]) :: xs ->
        let vbl_ml = [ Value_binding.eval vb ] in
        let body_ml = eval_block ~loc xs in
        E.pexp_let ~loc Asttypes.Nonrecursive vbl_ml body_ml
      (* "let" "rec" ... "," ... *)
      | Shape (loc, "rec", [ Scope ("(", Shape (_, ",", vbl), ")") ]) :: xs
      | Shape (loc, "rec", [ Shape (_, ",", vbl) ]) :: xs ->
        let vbl_ml = List.map Value_binding.eval vbl in
        let body_ml = eval_block ~loc xs in
        E.pexp_let ~loc Asttypes.Recursive vbl_ml body_ml
      (* rec _ *)
      | Shape (loc, "rec", [ vb ]) :: xs ->
        let vbl_ml = [ Value_binding.eval vb ] in
        let body_ml = eval_block ~loc xs in
        E.pexp_let ~loc Asttypes.Recursive vbl_ml body_ml
      | Shape (loc, "open", [ mexpr ]) :: items ->
        let mexpr = Module_expr.eval mexpr in
        let decl = E.open_infos ~loc ~expr:mexpr ~override:Asttypes.Fresh in
        E.pexp_open ~loc decl (eval_block ~loc items)
      | item :: items -> E.pexp_sequence ~loc (eval item) (eval_block ~loc items)

    and eval_let_in_unit ~loc rec_flag vbl_fl =
      let vbl_ml = List.map Value_binding.eval vbl_fl in
      E.pexp_let ~loc rec_flag vbl_ml (pexp_unit ~loc)

    and eval_list ~loc ?spread:tl xs =
      match xs with
      | [] -> Option.fold ~some:eval ~none:(pexp_nil ~loc) tl
      | x_fl :: xs_fl ->
        let x_ml = eval x_fl in
        let xs_ml = eval_list ~loc ?spread:tl xs_fl in
        pexp_cons ~loc x_ml xs_ml
  end

  and Pattern : sig
    val eval : fl -> E.pattern
  end = struct
    let rec eval (fl : fl) =
      match fl with
      (* _ *)
      | Ident (Lower "_") -> E.ppat_any ~loc:noloc
      (* a *)
      | Ident (Lower id) -> eval_var ~loc:noloc id
      (* --- unit --- *)
      | Scope ("(", Shape (_, ",", []), ")") | Scope ("(", Seq [], ")") ->
        pat_construct (with_noloc (Ident.Lident "()")) []
      (* --- tuple --- *)
      | Scope ("(", Shape (_, ",", items), ")") ->
        E.ppat_tuple ~loc:noloc (List.map eval items)
      (* (_) *)
      | Scope ("(", x, ")") -> eval x
      (* p : t *)
      | Shape (loc, ":", [ pat; typ ]) ->
        let typ = Core_type.eval typ in
        let pat = Pattern.eval pat in
        E.ppat_constraint ~loc pat typ
      (* --- const --- *)
      | Const const -> E.ppat_constant ~loc:noloc (conv_const const)
      (* --- list --- *)
      (* [a, b & tl] *)
      | Scope ("[", Shape (_loc, "&", [ Shape (_, ",", items); tl ]), "]") ->
        pat_list ~loc:noloc ?spread:(Some tl) items
      (* [a & tl] *)
      | Scope ("[", Shape (_loc, "&", [ a; tl ]), "]") ->
        pat_list ~loc:noloc ?spread:(Some tl) [ a ]
      (* [a, b, c] --- *)
      | Scope ("[", Shape (_, ",", items), "]") ->
        pat_list ~loc:noloc ?spread:None items
      (* [] *)
      | Scope ("[", Seq [], "]") -> pat_list ~loc:noloc ?spread:None []
      (* [a] *)
      | Scope ("[", item, "]") -> pat_list ~loc:noloc ?spread:None [ item ]
      (* A *)
      | Ident (Upper a) -> pat_construct (with_noloc (Ident.Lident a)) []
      (* A ... *)
      | Seq (Ident (Upper a) :: args) ->
        pat_construct (with_noloc (Ident.Lident a)) args
      (* _ as _ *)
      | Shape (loc, "as", [ pat; Ident (Lower alias) ]) ->
        let pat = eval pat in
        let alias = with_loc loc alias in
        E.ppat_alias ~loc pat alias
      (* _ | _ *)
      | Shape (loc, "_|_", [ a; b ]) ->
        let a = eval a in
        let b = eval b in
        E.ppat_or ~loc a b
      (* other *)
      | Shape (loc, _, _) ->
        (* FIXME *)
        Fmt.epr "todo pat: %a: %a@." Location.print_loc (Obj.magic loc)
          Shaper.dump fl;
        assert false
      | _ ->
        Fmt.epr "todo pat: %a@." Shaper.dump fl;
        assert false

    and pat_construct id args_fl =
      match args_fl with
      | [] -> E.ppat_construct ~loc:noloc id None
      | [ arg_fl ] ->
        let arg_ml = eval arg_fl in
        (* TODO t vars? *)
        E.ppat_construct ~loc:noloc id (Some arg_ml)
      | _ ->
        (* [TODO] Add explicity arity ext like Reason. *)
        let arg_ml = E.ppat_tuple ~loc:noloc (List.map eval args_fl) in
        E.ppat_construct ~loc:noloc id (Some arg_ml)

    and pat_list ~loc ?spread:tl xs =
      match xs with
      | [] -> Option.fold ~some:eval ~none:(ppat_nil ~loc) tl
      | x_fl :: xs_fl ->
        let x_ml = eval x_fl in
        let xs_ml = pat_list ~loc ?spread:tl xs_fl in
        ppat_cons ~loc x_ml xs_ml

    and eval_var ~loc id = E.ppat_var ~loc (with_loc loc id)
  end

  and Value_binding : sig
    val eval : fl -> E.value_binding
  end = struct
    let rec eval (fl : fl) =
      match fl with
      | Scope ("(", fl, ")") -> eval fl
      (* (_ : _) = _ *)
      | Shape (loc, "=", [ Shape (constraint_loc, ":", [ pat; typ ]); expr ]) ->
        let typ = Core_type.eval typ in
        let pat = Pattern.eval pat in
        let pat = E.ppat_constraint ~loc:constraint_loc pat typ in
        let expr = Expression.eval expr in
        E.value_binding ~loc ~pat ~expr
      (* _ = _ *)
      | Shape (loc, "=", [ pat; expr ]) ->
        let pat = Pattern.eval pat in
        let expr = Expression.eval expr in
        E.value_binding ~loc ~pat ~expr
      | _ ->
        Fmt.epr "not a vb: %a@." Shaper.dump fl;
        assert false
  end

  and Value_description : sig
    val eval : fl -> E.value_description
  end = struct
    let eval (fl : fl) =
      match fl with
      (* name : ... *)
      | Shape (loc, ":", [ Ident (Lower name); type' ]) ->
        let name = with_loc loc name in
        let type' = Core_type.eval type' in
        E.value_description ~loc ~name ~type_:type' ~prim:[]
      | _ ->
        Fmt.epr "not a vb: %a@." Shaper.dump fl;
        assert false
  end

  and Core_type : sig
    val eval : fl -> E.core_type
  end = struct
    let rec eval (fl : fl) : E.core_type =
      let loc = Ppxlib.Location.none in
      match fl with
      | Scope ("(", Shape (_, ",", items), ")") ->
        E.ptyp_tuple ~loc (List.map eval items)
      | Scope ("(", fl, ")") -> eval fl
      (* a *)
      | Ident (Lower id) -> E.ptyp_constr ~loc (with_noloc (Ident.Lident id)) []
      (* 'a *)
      | Shape (loc, "'", [ Ident (Lower id) ]) -> E.ptyp_var ~loc id
      (* M.a.b *)
      | Shape (loc, "ident", _) ->
        let ident = eval_expident fl in
        E.ptyp_constr ~loc ident []
      (* M.a.b ... *)
      | Seq ((Shape (loc, "ident", _) as ident) :: args) ->
        let ident = eval_expident ident in
        let args = List.map eval args in
        E.ptyp_constr ~loc ident args
      (* a ... *)
      | Seq (Ident (Lower id) :: args) ->
        let args = List.map eval args in
        E.ptyp_constr ~loc (with_noloc (Ident.Lident id)) args
      (* ~(a : t) -> _ *)
      | Shape
          ( loc
          , "->"
          , [ Shape
                ( _
                , "~"
                , [ Scope
                      ("(", Shape (_, ":", [ Ident (Lower label); arg ]), ")")
                  ]
                )
            ; ret
            ]
          ) ->
        let arg = eval arg in
        let ret = eval ret in
        E.ptyp_arrow ~loc (Asttypes.Labelled label) arg ret
      (* ~(a? : t) -> _ *)
      | Shape
          ( loc
          , "->"
          , [ Shape
                ( _
                , "~"
                , [ Scope
                      ( "("
                      , Shape
                          ( _
                          , ":"
                          , [ Shape (_, "?", [ Ident (Lower label) ]); arg ]
                          )
                      , ")"
                      )
                  ]
                )
            ; ret
            ]
          ) ->
        let arg = eval arg in
        let ret = eval ret in
        E.ptyp_arrow ~loc (Asttypes.Optional label) arg ret
      (* _ -> _ *)
      | Shape (loc, "->", [ arg; ret ]) ->
        let arg = eval arg in
        let ret = eval ret in
        E.ptyp_arrow ~loc Asttypes.Nolabel arg ret
      | _ ->
        Fmt.epr "%a@." Shaper.dump fl;
        failwith "todo: core type"
  end

  and Module_binding : sig
    val eval : fl -> E.module_binding
  end = struct
    let eval (fl : fl) =
      match fl with
      (* M = ... *)
      | Shape (loc, "=", [ Ident (Upper m_name); mexp_fl ]) ->
        let name = if String.equal m_name "_" then None else Some m_name in
        let name = with_loc loc name in
        let expr = Module_expr.eval mexp_fl in
        E.module_binding ~loc ~name ~expr
      (* M : ... = ... *)
      | Shape
          ( loc
          , "="
          , [ Shape (_loc, ":", [ Ident (Upper m_name); mtyp_fl ]); mexp_fl ]
          ) ->
        let name = if String.equal m_name "_" then None else Some m_name in
        let name = with_loc loc name in
        let expr = Module_expr.eval mexp_fl in
        let type' = Module_type.eval mtyp_fl in
        let expr = E.pmod_constraint ~loc expr type' in
        E.module_binding ~loc ~name ~expr
      | _ ->
        Fmt.epr "not a mb: %a@." Shaper.dump fl;
        assert false
  end

  and Module_expr : sig
    val eval : fl -> E.module_expr
  end = struct
    let eval (fl : fl) =
      match fl with
      | Ident (Upper id) ->
        E.pmod_ident ~loc:noloc (with_noloc (Ident.Lident id))
      (* M.M.M *)
      | Shape (loc, "ident", _) ->
        let ident = eval_expident fl in
        E.pmod_ident ~loc ident
      | Scope ("{", Seq [], "}") -> E.pmod_structure ~loc:noloc []
      | Scope ("{", Shape (loc, ";", items), "}") ->
        let items = List.map Structure_item.eval items in
        E.pmod_structure ~loc items
      | Scope ("{", item, "}") ->
        let items = [ Structure_item.eval item ] in
        E.pmod_structure ~loc:noloc items
      | _ ->
        Fmt.epr "todo: Mod:@.%a@." Shaper.dump fl;
        assert false
  end

  and Module_type : sig
    val eval : fl -> E.module_type
  end = struct
    let eval (fl : fl) =
      match fl with
      | Ident (Upper id) ->
        E.pmty_ident ~loc:noloc (with_noloc (Ident.Lident id))
      | Scope ("{", Seq [], "}") -> E.pmty_signature ~loc:noloc []
      | Scope ("{", Shape (loc, ";", items), "}") ->
        let items = List.map Signature_item.eval items in
        E.pmty_signature ~loc items
      | Scope ("{", item, "}") ->
        let items = [ Signature_item.eval item ] in
        E.pmty_signature ~loc:noloc items
      | _ ->
        Fmt.epr "todo: Mty:@.%a@." Shaper.dump fl;
        assert false
  end

  and Structure_item : sig
    val eval : fl -> E.structure_item
  end = struct
    let rec eval (fl : fl) =
      match fl with
      (* let f ... = _ *)
      (* let (+) ... = _ *)
      | Shape
          ( loc
          , "let"
          , [ Shape
                ( _
                , "="
                , [ Seq
                      ((Ident (Lower ident) | Scope ("(", Sym ident, ")"))
                      :: args
                      )
                  ; body
                  ]
                )
            ]
          ) -> eval_item_value_fn ~loc Asttypes.Nonrecursive ident args body
      (* let _ = _ *)
      | Shape (loc, "let", [ (Shape (_, "=", [ _lhs; _rhs ]) as vb) ])
      (* _ = _ *)
      (* | Shape (loc, "=", [ _lhs; _rhs ])  *) ->
        eval_item_value ~loc Asttypes.Nonrecursive [ vb ]
      (* _ = _, _ = _ *)
      (* | Shape (loc, "val", [ Shape (_, "=", [ _lhs; _rhs ]) ]) *)
      | Shape (loc, "let", [ Shape (_loc, ",", vbl) ]) ->
        eval_item_value ~loc Asttypes.Nonrecursive vbl
      (* rec _ = _ *)
      | Shape (loc, "rec", [ (Shape (_loc, "=", [ _lhs; _rhs ]) as vb) ]) ->
        eval_item_value ~loc Asttypes.Recursive [ vb ]
      (* rec _ = _, _ = _ *)
      | Shape (loc, "rec", [ Shape (_loc, ",", vbl) ]) ->
        eval_item_value ~loc Asttypes.Recursive vbl
      (* mod _ = _ *)
      | Shape (loc, ("module" | "mod"), [ mb ]) -> eval_item_module ~loc mb
      | Shape (loc, "sig", [ Shape (_, "=", [ Ident (Upper name); mod_ty ]) ])
        -> eval_item_module_type ~loc ~name mod_ty
      | Shape (loc, "open", [ mexp ]) -> eval_item_open ~loc mexp
      | Shape (loc, "do", [ exp ]) ->
        let unit = Shaper.parens (Shaper.seq []) in
        let vb = Shaper.shape "=" [ unit; exp ] in
        eval_item_value ~loc Asttypes.Nonrecursive [ vb ]
      (* --- type --- *)
      | Shape (loc, "type", args) ->
        eval_type ~loc ~private_flag:Asttypes.Public args
      (* --- type attributes --- *)
      (* | Shape (loc, "@", []) *)
      (* --- *)
      | Ident _ | Const _ | Sym _ | Seq (_ :: _) | Scope _ | Seq _ ->
        eval_eval ~loc:noloc fl
      | Shape (_loc, _kwd, _xs) ->
        Fmt.epr "Eval.str: %a@." Shaper.dump fl;
        assert false

    and eval_item_value ~loc rec_flag fl =
      let vbl = List.map Value_binding.eval fl in
      E.pstr_value ~loc rec_flag vbl

    and eval_item_value_fn ~loc rec_flag ident args body =
      let pat = E.ppat_var ~loc (with_loc loc ident) in
      let expr = Expression.eval_fn args body in
      let vbl = [ E.value_binding ~loc ~pat ~expr ] in
      E.pstr_value ~loc rec_flag vbl

    and eval_item_module ~loc mb_fl =
      let mb_ml = Module_binding.eval mb_fl in
      E.pstr_module ~loc mb_ml

    and eval_item_module_type ~loc ~name modty_fl =
      let modty_ml = Module_type.eval modty_fl in
      let mod_ty_decl =
        E.module_type_declaration ~loc ~name:(with_loc loc name)
          ~type_:(Some modty_ml)
      in
      E.pstr_modtype ~loc mod_ty_decl

    and eval_item_open ~loc expr =
      let expr = Module_expr.eval expr in
      let decl = E.open_infos ~loc ~expr ~override:Asttypes.Fresh in
      E.pstr_open ~loc decl

    and eval_eval ~loc fl = E.pstr_eval ~loc (Expression.eval fl) []

    and eval_constructor_declaration ~loc (fl : fl) =
      match fl with
      (* A *)
      | Ident (Upper a) ->
        E.constructor_declaration ~loc ~name:(with_noloc a)
          ~args:(E.pcstr_tuple []) ~res:None
      (* A ... *)
      | Seq (Ident (Upper a) :: args) ->
        let args = List.map Core_type.eval args in
        E.constructor_declaration ~loc ~name:(with_noloc a)
          ~args:(E.pcstr_tuple args) ~res:None
      | _ -> assert false

    and eval_label_declaration ~loc (fl : fl) =
      match fl with
      (* ... : mutable t *)
      | Shape
          ( loc
          , ":"
          , [ Ident (Lower name); Seq [ Ident (Lower "mutable"); type' ] ]
          ) ->
        let type' = Core_type.eval type' in
        E.label_declaration ~loc ~name:(with_loc loc name)
          ~mutable_:Asttypes.Mutable ~type_:type'
      (* err: ... : mutable ... *)
      | Shape
          (_loc, ":", [ Ident (Lower name); Seq (Ident (Lower "mutable") :: _) ])
        -> Fmt.failwith "invalid mutable record field syntax: %s" name
      (* ... : ... *)
      | Shape (loc, ":", [ Ident (Lower name); type' ]) ->
        let type' = Core_type.eval type' in
        E.label_declaration ~loc ~name:(with_loc loc name)
          ~mutable_:Asttypes.Immutable ~type_:type'
      | _ -> assert false

    and eval_type_kind ~loc (fl : fl) =
      match fl with
      (* A *)
      (* { A } *)
      | Ident (Upper a) | Scope ("{", Ident (Upper a), "}") ->
        let cd =
          E.constructor_declaration ~loc ~name:(with_noloc a)
            ~args:(E.pcstr_tuple []) ~res:None
        in
        E.ptype_variant [ cd ]
      (* A ... *)
      (* { A ... } *)
      | Seq (Ident (Upper a) :: args)
      | Scope ("{", Seq (Ident (Upper a) :: args), "}") ->
        let cd =
          let args = List.map Core_type.eval args in
          E.constructor_declaration ~loc ~name:(with_noloc a)
            ~args:(E.pcstr_tuple args) ~res:None
        in
        E.ptype_variant [ cd ]
      (* { ... | ... } *)
      | Scope ("{", Shape (loc, "_|_", cds_fl), "}") ->
        let cdl = List.map (eval_constructor_declaration ~loc) cds_fl in
        E.ptype_variant cdl
      (* { ... , ... } *)
      | Scope ("{", Shape (loc, ",", cds_fl), "}") ->
        let ldl = List.map (eval_label_declaration ~loc) cds_fl in
        E.ptype_record ldl
      (* .. *)
      | Sym ".." -> E.ptype_open
      (* a *)
      (* a t *)
      | _ -> E.ptype_abstract

    and make_type ~loc rec_flag name private_ (rhs : fl option) =
      let name = with_noloc name in
      let kind, manifest =
        match rhs with
        (* t == ... *)
        | Some (Seq [ Sym "=="; manifest_t; kind ]) ->
          let kind = eval_type_kind ~loc kind in
          let manifest = Core_type.eval manifest_t in
          (kind, Some manifest)
        (* ... *)
        | Some fl -> (
          match eval_type_kind ~loc fl with
          | Ml.Ptype_abstract as k -> (k, Some (Core_type.eval fl))
          | k -> (k, None)
        )
        | None -> (E.ptype_abstract, None)
      in
      let td =
        E.type_declaration ~loc:noloc ~name ~params:[] ~cstrs:[] ~kind ~private_
          ~manifest
      in
      E.pstr_type ~loc rec_flag [ td ]

    and eval_type ~loc ~private_flag (args : fl list) =
      match args with
      (* type t *)
      | [ Ident (Lower name) ] ->
        make_type ~loc Asttypes.Recursive name private_flag None
      (* type nonrec t. *)
      | [ Ident (Lower "nonrec"); Ident (Lower name) ] ->
        make_type ~loc Nonrecursive name private_flag None
      (* type t = ...rhs *)
      | [ Shape (loc, "=", [ Ident (Lower name); rhs ]) ] ->
        make_type ~loc Recursive name private_flag (Some rhs)
      (* type nonrec t = ... *)
      | [ Shape
            ( loc
            , "="
            , [ Seq [ Ident (Lower "nonrec"); Ident (Lower name) ]; rhs ]
            )
        ] -> make_type ~loc Nonrecursive name private_flag (Some rhs)
      | _ ->
        Fmt.epr "Eval.str: %a@." (Fmt.Dump.list Shaper.dump) args;
        assert false
  end

  and Signature_item : sig
    val eval : fl -> E.signature_item
  end = struct
    let rec eval (fl : fl) =
      match fl with
      (* val _ : _ *)
      | Shape (loc, ":", [ Ident (Lower name); type' ]) ->
        make_value ~loc ~name ~type'
      | _ ->
        Fmt.epr "Eval.sig: %a@." Shaper.dump fl;
        assert false

    and make_value ~loc ~name ~type' =
      let name = with_loc loc name in
      let type' = Core_type.eval type' in
      let vd = E.value_description ~loc ~name ~type_:type' ~prim:[] in
      E.psig_value ~loc vd
  end

  let structure (fl : fl) =
    match fl with
    | Shape (_loc, ";", items) -> List.map Structure_item.eval items
    | _ -> [ Structure_item.eval fl ]

  let expression = Expression.eval
  let pattern = Pattern.eval
  let core_type = Core_type.eval
  let value_binding = Value_binding.eval
  let module_binding = Module_binding.eval
  let structure_item = Structure_item.eval
end
