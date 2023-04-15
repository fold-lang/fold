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
    | Seq (None, items) ->
      construct "Seq" [ construct "None" []; list (List.map quasiquote items) ]
    | Seq (Some sep, items) ->
      construct "Seq"
        [ construct "Some" [ string sep ]; list (List.map quasiquote items) ]
    | Scope (l, x, r) -> construct "Scope" [ string l; quasiquote x; string r ]
    | Shape (_loc, "unquote", [ code ]) -> code
    | Shape (_loc, kwd, items) ->
      let noloc = dot (upper "Shaper") (lower "noloc") in
      construct "Shape" [ noloc; string kwd; list (List.map quasiquote items) ]

  module rec Expression : sig
    val eval : fl -> E.expression
  end = struct
    let rec eval (fl : fl) =
      match fl with
      (* --- ident --- *)
      | Ident (Lower x) -> E.pexp_ident ~loc:noloc (with_noloc (Ident.Lident x))
      | Sym x -> E.pexp_ident ~loc:noloc (with_noloc (Ident.Lident x))
      (* M.x *)
      | Shape (_loc, ".", [ Ident (Upper m); Ident (Lower v) ]) ->
        E.pexp_ident ~loc:noloc (with_noloc (Ident.Ldot (Lident m, v)))
      (* a.b *)
      | Shape (loc, ".", [ a; Ident (Lower id) ]) ->
        let a = eval a in
        E.pexp_field ~loc a (with_noloc (Ident.Lident id))
      (* -- let_in_unit -- *)
      | Shape (loc, "let", [ Seq (Some ",", vbl) ]) -> eval_let_in_unit ~loc vbl
      | Shape (loc, "let", [ Scope ("(", Seq (Some ",", vbl), ")") ]) ->
        eval_let_in_unit ~loc vbl
      | Shape (loc, "let", [ vb ]) -> eval_let_in_unit ~loc [ vb ]
      (* --- const --- *)
      | Const const -> E.pexp_constant ~loc:noloc (conv_const const)
      (* --- fn --- *)
      | Shape (_loc, "->", [ Seq (None, args); body ]) -> eval_fn args body
      | Shape (_loc, "->", [ arg; body ]) -> eval_fn [ arg ] body
      | Scope ("{", Shape (_loc, "->", [ arg; body ]), "}") ->
        eval_fn [ arg ] body
      (* --- fn_match --- *)
      | Scope
          ( "{"
          , Seq (Some ",", (Shape (_loc, "->", [ _; _ ]) :: _ as cases))
          , "}"
          ) -> eval_fn_match cases
      (* -- record -- *)
      (* `{ ~l, ...}` or `{ l = e, ... }` *)
      | Scope
          ("{", Seq (Some ",", (Shape (_, ("~" | "="), _) :: _ as fields)), "}")
        -> eval_record fields
      (* `{ r & ~l, ...}` or `{ r & l = e, ... }` *)
      | Scope
          ( "{"
          , Shape
              ( _
              , "&"
              , [ r
                ; Seq (Some ",", (Shape (_, ("~" | "="), _) :: _ as fields))
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
      | Shape (loc, "if", [ Scope ("{", Seq (Some ",", cases), "}") ]) ->
        eval_if_cases ~loc cases
      | Shape (loc, "if", [ Scope ("{", c, "}") ]) -> eval_if_cases ~loc [ c ]
      (* --- match --- *)
      | Shape (loc, "match", [ a; Scope ("{", Seq (Some ",", b), "}") ]) ->
        eval_match ~loc a b
      | Shape (loc, "match", [ _a; Scope ("{", Seq (None, []), "}") ]) ->
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
      (* -- try --- *)
      (* try _ { _, ... } *)
      | Shape (loc, "try", [ a; Scope ("{", Seq (Some ",", b), "}") ]) ->
        eval_try ~loc a b
      (* try _ { _ } *)
      | Shape (loc, "try", [ a; Scope ("{", b, "}") ]) -> eval_try ~loc a [ b ]
      (* --- construct --- *)
      (* TODO: explicit arity *)
      | Ident (Upper id) -> eval_construct ~loc:noloc (Ident.Lident id) []
      (* () *)
      | Scope ("(", Seq (None, []), ")") ->
        eval_construct ~loc:noloc (Ident.Lident "()") []
      (* {} *)
      | Scope ("{", Seq (None, []), "}") ->
        eval_construct ~loc:noloc (Ident.Lident "()") []
      | Seq (None, Ident (Upper c) :: args) ->
        eval_construct ~loc:noloc (Ident.Lident c) args
      (* --- polyvar --- *)
      (* TODO: explicit arity *)
      | Seq (None, Shape (loc, "#", [ Ident (Upper id) ]) :: args) ->
        eval_variant ~loc id args
      | Shape (loc, "#", [ Ident (Upper id) ]) -> eval_variant ~loc id []
      | Shape (_loc, "#", _) -> failwith "invalid polyvar syntax"
      (* --- apply or macro --- *)
      | Seq (None, f :: args) -> eval_apply f args
      (* --- a; b; b --- *)
      | Seq (Some ";", xs) -> eval_block xs
      (* Err: a, b *)
      | Seq (Some _sep, _) -> assert false
      (* --- () --- *)
      | Scope ("(", Seq (Some ",", []), ")") ->
        eval_construct ~loc:noloc (Ident.Lident "()") []
      | Scope ("(", Seq (Some ",", items), ")") -> eval_tuple ~loc:noloc items
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
      | Scope ("[", Shape (_loc, "&", [ Seq (Some ",", items); tl ]), "]") ->
        eval_list ~loc:noloc ?spread:(Some tl) items
      (* [a & tl] *)
      | Scope ("[", Shape (_loc, "&", [ a; tl ]), "]") ->
        eval_list ~loc:noloc ?spread:(Some tl) [ a ]
      (* [a, b, c] --- *)
      | Scope ("[", Seq (Some ",", items), "]") ->
        eval_list ~loc:noloc ?spread:None items
      (* [] *)
      | Scope ("[", Seq (None, []), "]") -> eval_list ~loc:noloc ?spread:None []
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
          (* Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl;
             Fmt.failwith "unknown macro %S" kwd *)
          let id = with_loc loc ("fl." ^ kwd) in
          let arg =
            match args with
            | [ x ] -> x
            | _ -> Shaper.seq args
          in
          let arg = Shaper.String (Fmt.str "%a" Shaper.pp arg) in
          let arg = E.pexp_constant ~loc (conv_const arg) in
          let item = E.pstr_eval ~loc arg [] in
          let payload = E.pstr [ item ] in
          E.pexp_extension ~loc (id, payload)
      end
      (* Err: unknown *)
      | _ ->
        Fmt.epr "--- unknown:@.%a@.---@." Shaper.dump fl;
        assert false

    and eval_apply f_fl args_fl =
      let f_ml = eval f_fl in
      let args_ml =
        List.map (fun arg_fl -> (Asttypes.Nolabel, eval arg_fl)) args_fl
      in
      E.pexp_apply ~loc:noloc f_ml args_ml

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

    and eval_fn (args_fl : fl list) (body_fl : fl) =
      let body_ml = eval body_fl in
      List.fold_left
        (fun acc arg_fl ->
          match (arg_fl : fl) with
          | Ident (Lower _id) ->
            let pat = Pattern.eval arg_fl in
            let label = Asttypes.Nolabel in
            let default = None in
            E.pexp_fun ~loc:noloc label default pat acc
          | _ ->
            Fmt.epr "@.args=%a@.body=%a@.@."
              (Fmt.Dump.list Shaper.dump)
              args_fl Shaper.dump body_fl;
            assert false
        )
        body_ml args_fl

    and eval_fn_match (cases_fl : fl list) =
      let cases = List.map eval_case cases_fl in
      E.pexp_function ~loc:noloc cases

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

    and eval_block (xs : fl list) =
      match xs with
      | [] -> pexp_unit ~loc:noloc
      | [ x ] -> eval x
      | Shape (loc, "let", [ Scope ("(", Seq (Some ",", vbl), ")") ]) :: xs
      | Shape (loc, "let", [ Seq (Some ",", vbl) ]) :: xs ->
        let vbl_ml = List.map Value_binding.eval vbl in
        let body_ml = eval_block xs in
        E.pexp_let ~loc Asttypes.Nonrecursive vbl_ml body_ml
      | Shape (loc, "let", [ vb ]) :: xs ->
        let vbl_ml = [ Value_binding.eval vb ] in
        let body_ml = eval_block xs in
        E.pexp_let ~loc Asttypes.Nonrecursive vbl_ml body_ml
      | Shape (loc, "open", [ mexpr ]) :: items ->
        let mexpr = Module_expr.eval mexpr in
        let decl = E.open_infos ~loc ~expr:mexpr ~override:Asttypes.Fresh in
        E.pexp_open ~loc decl (eval_block items)
      | item :: items ->
        E.pexp_sequence ~loc:noloc (eval item) (eval_block items)

    and eval_let_in_unit ~loc vbl_fl =
      let vbl_ml = List.map Value_binding.eval vbl_fl in
      E.pexp_let ~loc Asttypes.Nonrecursive vbl_ml (pexp_unit ~loc)

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
      | Ident (Lower id) -> eval_var ~loc:noloc id
      (* Unit *)
      | Scope ("(", Seq ((None | Some ","), []), ")") ->
        pat_construct (with_noloc (Ident.Lident "()")) []
      | Const const -> E.ppat_constant ~loc:noloc (conv_const const)
      (* --- list --- *)
      (* [a, b & tl] *)
      | Scope ("[", Shape (_loc, "&", [ Seq (Some ",", items); tl ]), "]") ->
        pat_list ~loc:noloc ?spread:(Some tl) items
      (* [a & tl] *)
      | Scope ("[", Shape (_loc, "&", [ a; tl ]), "]") ->
        pat_list ~loc:noloc ?spread:(Some tl) [ a ]
      (* [a, b, c] --- *)
      | Scope ("[", Seq (Some ",", items), "]") ->
        pat_list ~loc:noloc ?spread:None items
      (* [] *)
      | Scope ("[", Seq (None, []), "]") -> pat_list ~loc:noloc ?spread:None []
      (* [a] *)
      | Scope ("[", item, "]") -> pat_list ~loc:noloc ?spread:None [ item ]
      | Sym _ -> assert false
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
      (* `a = b` *)
      | Shape (loc, "=", [ pat; expr ]) ->
        let pat = Pattern.eval pat in
        let expr = Expression.eval expr in
        E.value_binding ~loc ~pat ~expr
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
      | Scope ("(", fl, ")") -> eval fl
      | Ident (Lower id) -> E.ptyp_constr ~loc (with_noloc (Ident.Lident id)) []
      | Seq (None, Ident (Lower id) :: args) ->
        let args = List.map eval args in
        E.ptyp_constr ~loc (with_noloc (Ident.Lident id)) args
      | _ ->
        Fmt.epr "%a@." Shaper.dump fl;
        failwith "todo: core type"
  end

  and Module_binding : sig
    val eval : fl -> E.module_binding
  end = struct
    let eval (fl : fl) =
      match fl with
      (* `a = b` *)
      | Shape (loc, "=", [ Ident (Upper m_name); mexp_fl ]) ->
        let name = if String.equal m_name "_" then None else Some m_name in
        let name = with_loc loc name in
        let expr = Module_expr.eval mexp_fl in
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
      | Scope ("{", Seq (None, []), "}") -> E.pmod_structure ~loc:noloc []
      | Scope ("{", Seq (Some ";", items), "}") ->
        let items = List.map Structure_item.eval items in
        E.pmod_structure ~loc:noloc items
      | _ ->
        Fmt.epr "todo: Mod:@.%a@." Shaper.dump fl;
        assert false
  end

  and Structure_item : sig
    val eval : fl -> E.structure_item
  end = struct
    let rec eval (fl : fl) =
      match fl with
      | Shape (loc, "=", [ _lhs; _rhs ]) -> eval_item_value ~loc [ fl ]
      | Seq (Some ",", vbl) -> eval_item_value ~loc:noloc vbl
      (* | Shape (_loc,"let", [ Seq (Some ",", vbl) ]) -> value vbl *)
      (* | Shape (_loc,"let", [ vb ]) -> value [ vb ] *)
      | Shape (loc, "module", [ mb ]) -> eval_item_module ~loc mb
      | Shape (loc, "open", [ mexp ]) -> eval_item_open ~loc mexp
      | Shape (loc, "do", [ exp ]) ->
        let unit = Shaper.parens (Shaper.seq []) in
        let vb = Shaper.shape "=" [ unit; exp ] in
        eval_item_value ~loc [ vb ]
      (* --- type --- *)
      | Shape (loc, "type", [ Seq (Some ",", tdl) ]) -> assert false
      | Shape (loc, "type", [ td ]) -> eval_type ~loc td
      | Shape (loc, "type", [ Ident (Lower "nonrec"); Ident (Lower name) ]) ->
        let name = with_noloc name in
        let td =
          E.type_declaration ~loc:noloc ~name ~params:[] ~cstrs:[]
            ~kind:E.ptype_abstract ~private_:Asttypes.Public ~manifest:None
        in
        E.pstr_type ~loc Asttypes.Nonrecursive [ td ]
      (* --- *)
      | Ident _ | Const _ | Sym _ | Seq (None, _ :: _) | Scope _ | Seq (None, _)
        -> eval_eval ~loc:noloc fl
      | Shape (_loc, _kwd, _xs) ->
        Fmt.epr "Eval.str: %a@." Shaper.dump fl;
        assert false
      | Seq (Some _sep, _) -> assert false

    and eval_item_value ~loc fl =
      let vbl = List.map Value_binding.eval fl in
      E.pstr_value ~loc Asttypes.Nonrecursive vbl

    and eval_item_module mb_fl =
      let mb_ml = Module_binding.eval mb_fl in
      E.pstr_module mb_ml

    and eval_item_open ~loc expr =
      let expr = Module_expr.eval expr in
      let decl = E.open_infos ~loc ~expr ~override:Asttypes.Fresh in
      E.pstr_open ~loc decl

    and eval_eval ~loc fl = E.pstr_eval ~loc (Expression.eval fl) []

    and eval_constructor_declaration ~loc (fl : fl) =
      match fl with
      | Ident (Upper a) ->
        E.constructor_declaration ~loc ~name:(with_noloc a)
          ~args:(E.pcstr_tuple []) ~res:None
      | Seq (None, Ident (Upper a) :: args) ->
        E.constructor_declaration ~loc ~name:(with_noloc a)
          ~args:(E.pcstr_tuple []) ~res:None
      | _ -> assert false

    and eval_type ~loc (fl0 : fl) =
      let mk ~loc rec_flag name private_ (rhs : fl option) =
        let name = with_noloc name in
        let kind, manifest =
          match rhs with
          | Some (Ident (Upper a) | Scope ("{", Ident (Upper a), "}")) ->
            let cd =
              E.constructor_declaration ~loc ~name:(with_noloc a)
                ~args:(E.pcstr_tuple []) ~res:None
            in
            let kind = E.ptype_variant [ cd ] in
            (kind, None)
          | Some
              ( Seq (None, Ident (Upper a) :: args)
              | Scope ("{", Seq (None, Ident (Upper a) :: args), "}") ) ->
            let cd =
              let args = E.pcstr_tuple (List.map Core_type.eval args) in
              E.constructor_declaration ~loc ~name:(with_noloc a) ~args
                ~res:None
            in
            let kind = E.ptype_variant [ cd ] in
            (kind, None)
          | Some fl -> (E.ptype_abstract, Some (Core_type.eval fl))
          | None -> (E.ptype_abstract, None)
        in
        let td =
          E.type_declaration ~loc:noloc ~name ~params:[] ~cstrs:[] ~kind
            ~private_ ~manifest
        in
        E.pstr_type ~loc rec_flag [ td ]
      in
      match fl0 with
      (* t *)
      | Ident (Lower name) -> mk ~loc Recursive name Public None
      (* nonrec t *)
      | Seq (None, [ Ident (Lower "nonrec"); Ident (Lower name) ]) ->
        mk ~loc Nonrecursive name Public None
      (* t = ... *)
      | Shape (loc, "=", [ Ident (Lower name); rhs ]) ->
        mk ~loc Recursive name Public (Some rhs)
      (* nonrec t = ... *)
      | Shape
          ( loc
          , "="
          , [ Seq (None, [ Ident (Lower "nonrec"); Ident (Lower name) ]); rhs ]
          ) -> mk ~loc Nonrecursive name Public (Some rhs)
      | _ ->
        Fmt.epr "%a@." Shaper.dump fl0;
        assert false

    and eval_type_declaration_rhs fl =
      match fl with
      | _ -> ()
  end

  let structure (fl : fl) =
    match fl with
    | Seq (Some ";", items) -> List.map Structure_item.eval items
    | _ -> [ Structure_item.eval fl ]

  let expression = Expression.eval
  let pattern = Pattern.eval
  let core_type = Core_type.eval
  let value_binding = Value_binding.eval
  let module_binding = Module_binding.eval
  let structure_item = Structure_item.eval
end
