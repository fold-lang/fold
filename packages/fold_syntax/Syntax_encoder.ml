let todo = Syntax.id "$SYNTAX"

type lid = Longident.t

let check_has_explicit_arity_attr attrs =
  List.exists
    (fun (attr : Parsetree.attribute) ->
      String.equal attr.attr_name.txt "explicit_arity")
    attrs

module rec Exp : sig
  val conv : Parsetree.expression -> Syntax.t

  val let_ :
       ?loc:'a
    -> ?attrs:'b
    -> Asttypes.rec_flag
    -> Parsetree.value_binding list
    -> Parsetree.expression
    -> Syntax.t

  val apply :
       ?loc:'a
    -> ?attrs:'b
    -> Parsetree.expression
    -> (Asttypes.arg_label * Parsetree.expression) list
    -> Syntax.t

  val tuple : ?loc:'a -> ?attrs:'b -> Parsetree.expression list -> Syntax.t

  val sequence :
       ?loc:'a
    -> ?attrs:'b
    -> Parsetree.expression
    -> Parsetree.expression
    -> Syntax.t

  val record :
       ?loc:'a
    -> ?attrs:'b
    -> (lid Location.loc * Parsetree.expression) list
    -> Parsetree.expression option
    -> Syntax.t

  val field : ?loc:'a -> ?attrs:'b -> Parsetree.expression -> lid -> Syntax.t

  val while_ :
       ?loc:'a
    -> ?attrs:'b
    -> Parsetree.expression
    -> Parsetree.expression
    -> Syntax.t

  val for_ :
       ?loc:'a
    -> ?attrs:'b
    -> Parsetree.pattern
    -> Parsetree.expression
    -> Parsetree.expression
    -> Asttypes.direction_flag
    -> Parsetree.expression
    -> Syntax.t
end = struct
  let flatten_map_list ~hd ~tl:tl0 f =
    let exception Not_a_list in
    let rec loop (tl : Parsetree.expression) acc =
      match tl.pexp_desc with
      | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev_map f acc
      | Pexp_construct
          ( { txt = Lident "::"; _ }
          , Some { pexp_desc = Pexp_tuple [ hd; tl' ]; _ } ) ->
        loop tl' (hd :: acc)
      | _ -> raise Not_a_list
    in
    try Some (loop tl0 [ hd ]) with Not_a_list -> None

  let rec conv (exp : Parsetree.expression) =
    let attrs = exp.pexp_attributes in
    match exp.pexp_desc with
    | Pexp_ident { txt = id; _ } -> Syntax.Id id
    | Pexp_constant constant -> Syntax.Const constant
    | Pexp_let (rec_flag, vbl, body) -> let_ rec_flag vbl body
    | Pexp_match (exp, cases) -> match_ exp cases
    | Pexp_apply (f_exp, args) -> apply f_exp args
    | Pexp_tuple items -> tuple items
    | Pexp_array items -> array items
    | Pexp_construct ({ txt = id; _ }, args) -> construct ~attrs id args
    | Pexp_record (bindings, r0) -> record bindings r0
    | Pexp_field (exp, { txt = id; _ }) -> field exp id
    | Pexp_sequence (exp_1, exp_2) -> sequence exp_1 exp_2
    | Pexp_while (cond, body) -> while_ cond body
    | Pexp_fun (l, default, pat, body) -> fun_ l default pat body
    | Pexp_for (i, from_exp, to_exp, dir_flag, body) ->
      for_ i from_exp to_exp dir_flag body
    | Pexp_ifthenelse (cond, if_true, if_false_opt) ->
      ifthenelse cond if_true if_false_opt
    | _ -> todo

  and let_ ?loc:_ ?attrs:_ rec_flag vbl body =
    let rec flatten (exp : Parsetree.expression) acc =
      match exp.pexp_desc with
      | Pexp_let (rec_flag, vbl, body) ->
        let vbl' = List.map Vb.conv vbl in
        let form =
          match rec_flag with
          | Nonrecursive -> Syntax_builder.let_ vbl'
          | Recursive -> Syntax_builder.let_rec vbl'
        in
        flatten body (form :: acc)
      | Pexp_sequence (exp_1, exp_2) -> flatten exp_2 (conv exp_1 :: acc)
      | _ -> List.rev (conv exp :: acc)
    in
    let vbl' = List.map Vb.conv vbl in
    let form =
      match rec_flag with
      | Nonrecursive -> Syntax_builder.let_ vbl'
      | Recursive -> Syntax_builder.let_rec vbl'
    in
    let body' = flatten body [] in
    Syntax_builder.block (form :: body')

  and apply ?loc:_ ?attrs:_ f_exp args =
    let f_exp' = conv f_exp in
    let args' = List.map conv_arg args in
    Syntax_builder.apply f_exp' args'

  and conv_arg (arg_label, exp) =
    match arg_label with
    | Labelled l -> Syntax_builder.label ~optional:false l (conv exp)
    | Optional l -> Syntax_builder.label ~optional:true l (conv exp)
    | Nolabel -> conv exp

  and tuple ?loc:_ ?attrs:_ items = Syntax_builder.tuple (List.map conv items)

  and sequence ?loc:_ ?attrs:_ exp0_1 exp0_2 =
    let rec flatten (exp : Parsetree.expression) acc =
      match exp.pexp_desc with
      | Pexp_sequence (exp_1, exp_2) -> flatten exp_2 (conv exp_1 :: acc)
      | _ -> List.rev (conv exp :: acc)
    in
    Syntax_builder.block (conv exp0_1 :: flatten exp0_2 [])

  and record ?loc:_ ?attrs:_ bindings r0 =
    let bindings' =
      List.map
        (fun ({ Location.txt = lid; _ }, v) ->
          Syntax_builder.binding (Syntax.Id lid) (conv v))
        bindings
    in
    match r0 with
    | Some r0 -> Syntax_builder.record (Some (conv r0)) bindings'
    | None -> Syntax_builder.record None bindings'

  and field ?loc:_ ?attrs:_ exp lid = Syntax_builder.field (conv exp) lid

  and while_ ?loc:_ ?attrs:_ cond body =
    Syntax_builder.while_ (conv cond) (conv body)

  and for_ ?loc:_ ?attrs:_ i from_exp to_exp dir_flag body =
    let i' = Pat.conv i in
    let from_exp' = conv from_exp in
    let to_exp' = conv to_exp in
    let body' = conv body in
    let down =
      match dir_flag with
      | Asttypes.Upto -> false
      | Asttypes.Downto -> true
    in
    Syntax_builder.for_ ~down
      (Syntax_builder.binding i' from_exp')
      to_exp' body'

  and ifthenelse ?loc:_ ?attrs:_ cond if_true if_false_opt =
    let cond' = conv cond in
    let if_true' = conv if_true in
    match if_false_opt with
    | Some if_false ->
      let if_false' = conv if_false in
      Syntax_builder.if_then_else cond' if_true' if_false'
    | None -> Syntax_builder.if_then cond' if_true'

  and conv_fun_arg (arg_label : Asttypes.arg_label) default pat =
    let pat' = Pat.conv pat in
    match arg_label with
    (* ~l or ~l:m *)
    | Labelled l -> Syntax_builder.label ~optional:false l pat'
    | Optional l -> (
      match default with
      (* ?l or ?l:m *)
      | None -> Syntax_builder.label ~optional:true l pat'
      (* ?(l = v) or ?l:(m = v) *)
      | Some x ->
        Syntax_builder.label ~optional:true l
          (Syntax_builder.binding pat' (conv x)))
    | Nolabel -> pat'

  and fun_ ?loc:_ ?attrs:_ l0 default0 pat0 body0 =
    let rec flatten (body : Parsetree.expression) acc =
      match body.pexp_desc with
      | Pexp_fun (l, default, pat, body) ->
        let arg = conv_fun_arg l default pat in
        flatten body (arg :: acc)
      | _ -> conv body :: List.rev acc
    in
    let arg0 = conv_fun_arg l0 default0 pat0 in
    match flatten body0 [ arg0 ] with
    | body :: args -> Syntax_builder.fn args body
    | _ -> failwith "impossible state: unexpected empty fun body"

  and array ?loc:_ ?attrs:_ items = Syntax_builder.array (List.map conv items)

  (* IMPORTANT: Keep in sync with Pat.construct *)
  and construct ?loc:_ ~attrs id args_opt =
    let has_explicit_arity = check_has_explicit_arity_attr attrs in
    let mk_args (args : Parsetree.expression) =
      match args.pexp_desc with
      | Pexp_tuple args when has_explicit_arity -> List.map conv args
      | _ -> [ conv args ]
    in
    match id with
    (* List constructors *)
    | Longident.Lident "[]" -> (
      match args_opt with
      (* [] *)
      | None -> Syntax_builder.list []
      (* [([]) x], i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args))
    | Longident.Lident "::" -> (
      match args_opt with
      (* [(::) (hd, tl)] *)
      | Some ({ pexp_desc = Pexp_tuple [ hd; tl ]; _ } as args) -> (
        match flatten_map_list ~hd ~tl conv with
        | Some list -> Syntax_builder.list list
        | None -> Syntax_builder.apply (Syntax.Id id) (mk_args args))
      (* [(::) x] where [x] is not (hd, tl), i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args)
      (* [(::)], i.e., not a list. *)
      | None -> Syntax_builder.apply (Syntax.Id id) [])
    (* Other constructors *)
    | _ -> (
      match args_opt with
      | None -> Syntax_builder.apply (Id id) []
      | Some args -> Syntax_builder.apply (Id id) (mk_args args))

  and case pat ?guard exp =
    let pat_syn = Pat.conv pat in
    let exp_syn = conv exp in
    let guard_syn = Option.map conv guard in
    Syntax_builder.case pat_syn ?guard:guard_syn exp_syn

  and match_ ?loc:_ ?attrs:_ exp cases =
    let exp_syn = conv exp in
    let cases_syn =
      Syntax_builder.or_
        (List.map
           (fun (c : Parsetree.case) ->
             case c.pc_lhs ?guard:c.pc_guard c.pc_rhs)
           cases)
    in
    Syntax_builder.match_ exp_syn cases_syn
end

and Pat : sig
  val conv : Parsetree.pattern -> Syntax.t
end = struct
  let flatten_map_list ~hd ~tl:tl0 f =
    let exception Not_a_list in
    let rec loop (tl : Parsetree.pattern) acc =
      match tl.ppat_desc with
      | Ppat_construct ({ txt = Lident "[]"; _ }, None) -> List.rev_map f acc
      | Ppat_construct
          ( { txt = Lident "::"; _ }
            (* XXX this is niche, ignore types for now in lists? *)
          , Some (_types, { ppat_desc = Ppat_tuple [ hd; tl' ]; _ }) ) ->
        loop tl' (hd :: acc)
      | _ -> raise Not_a_list
    in
    try Some (loop tl0 [ hd ]) with Not_a_list -> None

  let rec conv (pat : Parsetree.pattern) =
    let attrs = pat.ppat_attributes in
    match pat.ppat_desc with
    | Ppat_constant constant -> Syntax.Const constant
    | Ppat_var { txt = var; _ } -> Syntax.id var
    | Ppat_construct ({ txt = id; _ }, args) -> construct ~attrs id args
    | Ppat_tuple items -> tuple items
    | _ -> todo

  and tuple ?loc:_ ?attrs:_ items = Syntax_builder.tuple (List.map conv items)

  (* IMPORTANT: Keep in sync with Exp.construct *)
  and construct ?loc:_ ~attrs id args_opt =
    let has_explicit_arity = check_has_explicit_arity_attr attrs in
    let mk_args (_types, (args : Parsetree.pattern)) =
      match args.ppat_desc with
      | Ppat_tuple args when has_explicit_arity -> List.map conv args
      | _ -> [ conv args ]
    in
    match id with
    (* List constructors *)
    | Longident.Lident "[]" -> (
      match args_opt with
      (* [] *)
      | None -> Syntax_builder.list []
      (* [([]) x], i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args))
    | Longident.Lident "::" -> (
      match args_opt with
      (* [(::) (hd, tl)] *)
      | Some ((_types, { ppat_desc = Ppat_tuple [ hd; tl ]; _ }) as args) -> (
        match flatten_map_list ~hd ~tl conv with
        | Some list -> Syntax_builder.list list
        | None -> Syntax_builder.apply (Syntax.Id id) (mk_args args))
      (* [(::) x] where [x] is not (hd, tl), i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args)
      (* [(::)], i.e., not a list. *)
      | None -> Syntax_builder.apply (Syntax.Id id) [])
    (* Other constructors *)
    | _ -> (
      match args_opt with
      | None -> Syntax_builder.apply (Id id) []
      | Some args -> Syntax_builder.apply (Id id) (mk_args args))
end

and Vb : sig
  val conv : Parsetree.value_binding -> Syntax.t

  val mk :
       ?loc:'loc
    -> ?attrs:'attrs
    -> ?docs:'docs
    -> ?text:'text
    -> Parsetree.pattern
    -> Parsetree.expression
    -> Syntax.t
end = struct
  let mk ?loc:_ ?attrs:_ ?docs:_ ?text:_ pat exp =
    let pat' = Pat.conv pat in
    let exp' = Exp.conv exp in
    Syntax_builder.binding pat' exp'

  let conv (vb : Parsetree.value_binding) = mk vb.pvb_pat vb.pvb_expr
end

and Str : sig
  val mk : ?loc:'loc -> Parsetree.structure_item_desc -> Syntax.t
  val conv : Parsetree.structure_item -> Syntax.t
end = struct
  let rec mk ?loc:_ (str_item_desc : Parsetree.structure_item_desc) =
    match str_item_desc with
    | Pstr_eval (exp, _attrs) -> Exp.conv exp
    | Pstr_value (rec_flag, vbl) -> value rec_flag vbl
    | Pstr_primitive _val_desc -> todo
    | Pstr_type (_rec_flag, _tdl) -> todo
    | Pstr_typext _text -> todo
    | Pstr_exception _texn -> todo
    | Pstr_module mb -> module_ mb
    | Pstr_recmodule _mbl -> todo
    | Pstr_modtype _mtd -> todo
    | Pstr_open _od -> todo
    | Pstr_class _cd -> todo
    | Pstr_class_type _cdl -> todo
    | Pstr_include _incd -> todo
    | Pstr_attribute _attr -> todo
    | Pstr_extension (_ext, _attrs) -> todo

  and value ?loc:_ rec_flag vbl =
    let vbl' = List.map Vb.conv vbl in
    match rec_flag with
    | Asttypes.Nonrecursive -> Syntax_builder.let_ vbl'
    | Recursive -> Syntax_builder.let_rec vbl'

  and module_ ?loc:_ mb =
    let mb' = Mb.conv mb in
    Syntax_builder.module_ mb'

  let conv (str_item : Parsetree.structure_item) = mk str_item.pstr_desc
end

and Mb : sig
  val conv : Parsetree.module_binding -> Syntax.t

  val mk :
       ?loc:'loc
    -> ?attrs:'attrs
    -> ?docs:'docs
    -> ?text:'text
    -> Ast_helper.str_opt
    -> Parsetree.module_expr
    -> Syntax.t
end = struct
  let mk ?loc:_ ?attrs:_ ?docs:_ ?text:_ (str_opt : Ast_helper.str_opt) mod_exp
      =
    let mod_name =
      Syntax.id
        (match str_opt.txt with
        | None -> "_"
        | Some str -> str)
    in
    let mod_exp' = Mod.conv mod_exp in
    Syntax_builder.binding mod_name mod_exp'

  let conv (mb : Parsetree.module_binding) =
    mk ~loc:mb.pmb_loc ~attrs:mb.pmb_attributes mb.pmb_name mb.pmb_expr
end

and Mod : sig
  val mk : ?loc:'a -> ?attrs:'b -> Parsetree.module_expr_desc -> Syntax.t
  val conv : Parsetree.module_expr -> Syntax.t
end = struct
  let mk ?loc:_ ?attrs:_ (mod_exp_desc : Parsetree.module_expr_desc) =
    match mod_exp_desc with
    | Pmod_ident _id -> todo
    | Pmod_structure str -> Syntax_builder.block (List.map Str.conv str)
    | Pmod_functor (_functor_parameter, _body) -> todo
    | Pmod_apply (_f, _arg) -> todo
    (* | Pmod_apply_unit f -> todo *)
    | Pmod_constraint (_me, _mt) -> todo
    | Pmod_unpack _expr -> todo
    | Pmod_extension _ext -> todo

  let conv (mod_exp : Parsetree.module_expr) =
    mk ~loc:mod_exp.pmod_loc ~attrs:mod_exp.pmod_attributes mod_exp.pmod_desc
end

let structure str = Syntax_builder.block (List.map Str.conv str)
