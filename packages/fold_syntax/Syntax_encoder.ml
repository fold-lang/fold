type lid = Longident.t
type loc = Location.t
type 'a with_loc = 'a Location.loc

let check_has_explicit_arity_attr attrs =
  List.exists
    (fun (attr : Parsetree.attribute) ->
      String.equal attr.attr_name.txt "explicit_arity"
    )
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
  let todo what = Syntax.id ("$EXP: " ^ what)

  let flatten_map_list ~hd ~tl:tl0 f =
    let rec loop (tl : Parsetree.expression) acc =
      match tl.pexp_desc with
      | Pexp_construct ({ txt = Lident "[]"; _ }, None) ->
        (List.rev_map f acc, None)
      | Pexp_construct
          ( { txt = Lident "::"; _ }
          , Some { pexp_desc = Pexp_tuple [ hd; tl' ]; _ }
          ) -> loop tl' (hd :: acc)
      | _ -> (List.rev_map f acc, Some (f tl))
    in
    loop tl0 [ hd ]

  let rec conv (exp : Parsetree.expression) =
    let attrs = exp.pexp_attributes in
    match exp.pexp_desc with
    | Pexp_ident { txt = id; _ } -> Syntax.Id id
    | Pexp_constant constant -> Syntax.Const constant
    | Pexp_let (rec_flag, vbl, body) -> let_ rec_flag vbl body
    | Pexp_match (exp, cases) -> match_ exp cases
    | Pexp_try (exp, cases) -> todo "try"
    | Pexp_apply (f_exp, args) -> apply f_exp args
    | Pexp_tuple items -> tuple items
    | Pexp_array items -> array items
    | Pexp_construct ({ txt = id; _ }, args) -> construct ~attrs id args
    | Pexp_variant (label, arg) -> todo "polyvar"
    | Pexp_record (bindings, r0) -> record bindings r0
    | Pexp_field (exp, { txt = id; _ }) -> field exp id
    | Pexp_setfield (lexp, id, rexp) -> todo "setfield"
    | Pexp_sequence (exp_1, exp_2) -> sequence exp_1 exp_2
    | Pexp_fun (l, default, pat, body) -> fun_ l default pat body
    | Pexp_function cases -> function_ cases
    | Pexp_while (cond, body) -> while_ cond body
    | Pexp_for (i, from_exp, to_exp, dir_flag, body) ->
      for_ i from_exp to_exp dir_flag body
    | Pexp_ifthenelse (cond, if_true, if_false_opt) ->
      ifthenelse cond if_true if_false_opt
    | Pexp_constraint (exp, typ) -> constraint_ exp typ
    | Pexp_coerce (exp, typ_opt, typ) -> todo "coerce"
    | Pexp_send (exp, l) -> todo "send"
    | Pexp_new id -> todo "new"
    | Pexp_setinstvar (l, exp) -> todo "setinstvar"
    | Pexp_override items -> todo "override"
    | Pexp_letmodule (mod_name, mexpr, exp) -> todo "letmodule"
    | Pexp_letexception (ext_constr, exp) -> todo "letexception"
    | Pexp_assert exp -> todo "assert"
    | Pexp_lazy exp -> todo "lazy"
    | Pexp_poly (exp, typ_opt) -> todo "poly"
    | Pexp_object cls -> todo "object"
    | Pexp_newtype (typ_name, exp) -> todo "newtype"
    | Pexp_pack mexp -> todo "pack"
    | Pexp_open (decl, exp) -> open_ decl exp
    | Pexp_letop letop -> todo "letop"
    | Pexp_extension ext -> todo "extension"
    | Pexp_unreachable -> todo "unreachable"

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
      | Pexp_open (decl, exp) ->
        flatten exp (Syntax_builder.open_ (Mod.conv decl.popen_expr) :: acc)
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
          Syntax_builder.binding (Syntax.Id lid) (conv v)
        )
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
          (Syntax_builder.binding pat' (conv x))
    )
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

  and function_ cases =
    let cases_syn =
      Syntax_builder.or_
        (List.map
           (fun (c : Parsetree.case) -> case c.pc_lhs ?guard:c.pc_guard c.pc_rhs)
           cases
        )
    in
    Syntax_builder.fn [] cases_syn

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
    (* Empty list *)
    | Longident.Lident "[]" -> (
      match args_opt with
      (* [] *)
      | None -> Syntax_builder.list [] None
      (* [([]) x], i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args)
    )
    (* Non-empty list *)
    | Longident.Lident "::" -> (
      match args_opt with
      (* [(::) (hd, tl)] *)
      | Some { pexp_desc = Pexp_tuple [ hd; tl ]; _ } ->
        let items, tl = flatten_map_list ~hd ~tl conv in
        Syntax_builder.list items tl
      (* [(::) x] where [x] is not (hd, tl), i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args)
      (* [(::)], i.e., not a list. *)
      | None -> Syntax_builder.apply (Syntax.Id id) []
    )
    (* Other constructors *)
    | _ -> (
      match args_opt with
      | None -> Syntax_builder.apply (Id id) []
      | Some args -> Syntax_builder.apply (Id id) (mk_args args)
    )

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
           (fun (c : Parsetree.case) -> case c.pc_lhs ?guard:c.pc_guard c.pc_rhs)
           cases
        )
    in
    Syntax_builder.match_ exp_syn cases_syn

  and constraint_ ?loc:_ ?attrs:_ exp typ =
    let exp_syn = conv exp in
    let typ_syn = Typ.conv typ in
    Syntax_builder.constraint_ exp_syn typ_syn

  and open_ ?loc:_ ?attrs:_ (decl : Parsetree.open_declaration) exp =
    Syntax_builder.block
      [ Syntax_builder.open_ (Mod.conv decl.popen_expr); conv exp ]
end

and Val : sig
  type value_description = Syntax.t

  val mk :
       ?loc:loc
    -> ?attrs:Parsetree.attributes
    -> ?docs:Docstrings.docs
    -> ?prim:string list
    -> string with_loc
    -> Parsetree.core_type
    -> value_description

  val conv : Parsetree.value_description -> value_description
end = struct
  type value_description = Syntax.t

  let mk ?loc:_ ?attrs:_ ?docs:_ ?(prim = []) (name_with_loc : _ with_loc)
      (typ : Parsetree.core_type) =
    let name_syn = Syntax.id name_with_loc.txt in
    let cons_syn = Syntax_builder.constraint_ name_syn (Typ.conv typ) in
    match prim with
    | [] -> Syntax.Form [ Syntax.id "val"; cons_syn ]
    | _ -> Syntax.id "external"

  let conv (vdesc : Parsetree.value_description) =
    (* XXX docs *)
    mk ~loc:vdesc.pval_loc ~attrs:vdesc.pval_attributes ~prim:vdesc.pval_prim
      vdesc.pval_name vdesc.pval_type
end

and Typ : sig
  val conv : Parsetree.core_type -> Syntax.t
end = struct
  let todo what = Syntax.id ("XXX: " ^ what)

  let rec conv (typ : Parsetree.core_type) =
    match typ.ptyp_desc with
    | Ptyp_any -> Syntax_builder.id "_"
    | Ptyp_var var -> Syntax_builder.id ("'" ^ var)
    | Ptyp_arrow (arg_label, typ_1, typ_2) -> arrow arg_label typ_1 typ_2
    | Ptyp_tuple items -> tuple items
    | Ptyp_constr (id, typs) -> constr id typs
    | Ptyp_object (object_field_list, closed_flag) -> todo "object"
    | Ptyp_class (id, core_type_list) -> todo "class"
    | Ptyp_alias (core_type, string) -> todo "alias"
    | Ptyp_variant (row_fields, closed_flag, label_list_option) ->
      todo "variant"
    | Ptyp_poly (vars, typ) -> poly vars typ
    | Ptyp_package package_type -> todo "package"
    | Ptyp_extension extension -> todo "extension"

  and tuple items = Syntax_builder.tuple (List.map conv items)

  and constr ?loc:_ ?attrs:_ (id : _ Location.loc) typs =
    let typs_syn = List.map conv typs in
    Syntax_builder.apply (Syntax.Id id.txt) typs_syn

  and arrow ?loc:_ ?attrs:_ arg_label typ_1 typ_2 =
    let typ_1_syn = conv typ_1 in
    let typ_2_syn = conv typ_2 in
    let typ_1_arg =
      match arg_label with
      | Labelled l -> Syntax_builder.label ~optional:false l typ_1_syn
      | Optional l -> Syntax_builder.label ~optional:true l typ_1_syn
      | Nolabel -> typ_1_syn
    in
    Syntax_builder.arrow typ_1_arg typ_2_syn

  and poly ?loc:_ ?attrs:_ vars typ =
    match vars with
    | [] -> conv typ
    | vars ->
      let items =
        Syntax.id "forall"
        :: List.map
             (fun { Location.txt = var; _ } -> Syntax.id ("'" ^ var))
             vars
      in
      Syntax.Form (List.append items [ Syntax.id "."; conv typ ])
end

and Pat : sig
  val conv : Parsetree.pattern -> Syntax.t
end = struct
  let todo what = Syntax.id ("XXX: " ^ what)

  let flatten_map_list ~hd ~tl:tl0 f =
    let rec loop (tl : Parsetree.pattern) acc =
      match tl.ppat_desc with
      | Ppat_construct ({ txt = Lident "[]"; _ }, None) ->
        (List.rev_map f acc, None)
      | Ppat_construct
          ( { txt = Lident "::"; _ }
            (* XXX this is niche, ignore types for now in lists? *)
          , Some (_types, { ppat_desc = Ppat_tuple [ hd; tl' ]; _ })
          ) -> loop tl' (hd :: acc)
      | _ -> (List.rev_map f acc, Some (f tl))
    in
    loop tl0 [ hd ]

  let rec conv (pat : Parsetree.pattern) =
    let attrs = pat.ppat_attributes in
    match pat.ppat_desc with
    | Ppat_constant constant -> Syntax.Const constant
    | Ppat_any -> Syntax.id "_"
    | Ppat_var { txt = var; _ } -> Syntax.id var
    | Ppat_construct ({ txt = id; _ }, args) -> construct ~attrs id args
    | Ppat_constraint (pat, ctype) -> constraint_ pat ctype
    | Ppat_tuple items -> tuple items
    | Ppat_record (fields, closed_flag) -> record fields closed_flag
    | Ppat_alias (_, _) -> todo "alias"
    | Ppat_interval (_, _) -> todo "interval"
    | Ppat_variant (_, _) -> todo "variant"
    | Ppat_array _ -> todo "array"
    | Ppat_or (pat_1, pat_2) -> or_ pat_1 pat_2
    | Ppat_type _ -> todo "type"
    | Ppat_lazy _ -> todo "lazy"
    | Ppat_unpack _ -> todo "unpack"
    | Ppat_exception _ -> todo "exception"
    | Ppat_extension _ -> todo "extension"
    | Ppat_open (_, _) -> todo "open"

  and tuple ?loc:_ ?attrs:_ items = Syntax_builder.tuple (List.map conv items)

  and record ?loc:_ ?attrs:_ fields closed_flag =
    let fields_syn =
      List.map
        (fun ({ Location.txt = id; _ }, p) ->
          Syntax_builder.binding (Syntax.Id id) (Pat.conv p)
        )
        fields
    in
    match closed_flag with
    | Closed -> Syntax_builder.record None fields_syn
    | Open -> Syntax_builder.record (Some (Syntax_builder.id "_")) fields_syn

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
      | None -> Syntax_builder.list [] None
      (* [([]) x], i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args)
    )
    | Longident.Lident "::" -> (
      match args_opt with
      (* [(::) (hd, tl)] *)
      | Some (_types, { ppat_desc = Ppat_tuple [ hd; tl ]; _ }) ->
        let items, tl = flatten_map_list ~hd ~tl conv in
        Syntax_builder.list items tl
      (* [(::) x] where [x] is not (hd, tl), i.e., not a list. *)
      | Some args -> Syntax_builder.apply (Syntax.Id id) (mk_args args)
      (* [(::)], i.e., not a list. *)
      | None -> Syntax_builder.apply (Syntax.Id id) []
    )
    (* Other constructors *)
    | _ -> (
      match args_opt with
      | None -> Syntax_builder.apply (Id id) []
      | Some args -> Syntax_builder.apply (Id id) (mk_args args)
    )

  and constraint_ ?loc:_ ?attrs:_ pat typ =
    let pat_syn = Pat.conv pat in
    let typ_syn = Typ.conv typ in
    Syntax_builder.constraint_ pat_syn typ_syn

  and or_ pat_1 pat_2 =
    let rec flatten (pat : Parsetree.pattern) acc =
      match pat.ppat_desc with
      | Ppat_or (pat', x) -> flatten pat' (conv x :: acc)
      | _ -> conv pat :: acc
    in
    let pats = flatten pat_1 [ conv pat_2 ] in
    Syntax_builder.or_ pats
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
  let conv_fun_arg (arg_label : Asttypes.arg_label) default pat =
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
          (Syntax_builder.binding pat' (Exp.conv x))
    )
    | Nolabel -> pat'

  let mk ?loc:_ ?attrs:_ ?docs:_ ?text:_ (pat : Parsetree.pattern)
      (exp : Parsetree.expression) =
    let pat_syn = Pat.conv pat in
    match (pat.ppat_desc, exp.pexp_desc) with
    (* Both pat and exp have the the type constraint. *)
    | ( Ppat_constraint (_, { ptyp_desc = _pat_t; _ })
      , Pexp_constraint (exp', { ptyp_desc = _exp_t; _ }) ) ->
      (* Can pat_t and exp_t ever be different? *)
      let exp'_syn = Exp.conv exp' in
      Syntax_builder.binding pat_syn exp'_syn
    | _, Pexp_fun (l0, default0, pat0, body0) ->
      let rec flatten (body : Parsetree.expression) acc =
        match body.pexp_desc with
        | Pexp_fun (l, default, pat, body) ->
          let arg = conv_fun_arg l default pat in
          flatten body (arg :: acc)
        | Pexp_constraint (body, typ) ->
          let args = List.rev acc in
          let body = Exp.conv body in
          let apply_syn = Syntax_builder.apply pat_syn args in
          let pat = Syntax_builder.constraint_ apply_syn (Typ.conv typ) in
          Syntax_builder.binding pat body
        | _ ->
          let args = List.rev acc in
          let body = Exp.conv body in
          Syntax_builder.binding (Syntax_builder.apply pat_syn args) body
      in
      let arg0 = conv_fun_arg l0 default0 pat0 in
      flatten body0 [ arg0 ]
    | _ ->
      let exp_syn = Exp.conv exp in
      Syntax_builder.binding pat_syn exp_syn

  let conv (vb : Parsetree.value_binding) = mk vb.pvb_pat vb.pvb_expr
end

and Str : sig
  val mk : ?loc:'loc -> Parsetree.structure_item_desc -> Syntax.t
  val conv : Parsetree.structure_item -> Syntax.t
end = struct
  let todo what = Syntax.id ("$STR: " ^ what)

  let rec mk ?loc:_ (str_item_desc : Parsetree.structure_item_desc) =
    match str_item_desc with
    | Pstr_eval (exp, _attrs) -> Exp.conv exp
    | Pstr_value (rec_flag, vbl) -> value rec_flag vbl
    | Pstr_primitive _val_desc -> todo "primitive"
    | Pstr_type (_rec_flag, _tdl) -> todo "type"
    | Pstr_typext _text -> todo "typext"
    | Pstr_exception _texn -> todo "exception"
    | Pstr_module mb -> module_ mb
    | Pstr_recmodule mbl -> rec_module mbl
    | Pstr_modtype _mtd -> todo "modtype"
    | Pstr_open decl -> Syntax_builder.open_ (Mod.conv decl.popen_expr)
    | Pstr_class _cd -> todo "class"
    | Pstr_class_type _cdl -> todo "class_type"
    | Pstr_include _incd -> todo "include"
    | Pstr_attribute _attr -> todo "attribute"
    | Pstr_extension (_ext, _attrs) -> todo "extension"

  and value ?loc:_ rec_flag vbl =
    let vbl' = List.map Vb.conv vbl in
    match rec_flag with
    | Asttypes.Nonrecursive -> Syntax_builder.val_ vbl'
    | Recursive -> Syntax_builder.val_rec vbl'

  and module_ ?loc:_ mb =
    let mb' = Mb.conv mb in
    Syntax_builder.module_ mb'

  and rec_module ?loc:_ mbl =
    let mbl_syn = List.map Mb.conv mbl in
    Syntax_builder.rec_module mbl_syn

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
  let mk ?loc:_ ?attrs:_ ?docs:_ ?text:_ (str_opt : Ast_helper.str_opt) mexp =
    let mod_name =
      Syntax.id
        ( match str_opt.txt with
        | None -> "_"
        | Some str -> str
        )
    in
    match mexp.Parsetree.pmod_desc with
    | Pmod_constraint (mexp, mtyp) ->
      let mexp_sync = Mod.conv mexp in
      let mtyp_sync = Mty.conv mtyp in
      Syntax_builder.binding
        (Syntax_builder.constraint_ mod_name mtyp_sync)
        mexp_sync
    | _ ->
      let mexp_syn = Mod.conv mexp in
      Syntax_builder.binding mod_name mexp_syn

  let conv (mb : Parsetree.module_binding) =
    mk ~loc:mb.pmb_loc ~attrs:mb.pmb_attributes mb.pmb_name mb.pmb_expr
end

and Mod : sig
  type module_expr = Syntax.t

  val mk :
       ?loc:Location.t
    -> ?attrs:Parsetree.attributes
    -> Parsetree.module_expr_desc
    -> module_expr

  val conv : Parsetree.module_expr -> Syntax.t
end = struct
  type module_expr = Syntax.t

  let todo ?(what = "?") () = Syntax.id ("$MOD: " ^ what)

  let rec mk ?loc:_ ?attrs:_ (mexp_desc : Parsetree.module_expr_desc) =
    match mexp_desc with
    | Pmod_ident { txt = id; _ } -> Syntax.Id id
    | Pmod_structure str -> Syntax_builder.block (List.map Str.conv str)
    | Pmod_functor (_functor_parameter, _body) -> todo ~what:"functor " ()
    | Pmod_apply (_f, _arg) -> todo ~what:"apply " ()
    (* | Pmod_apply_unit f -> todo () *)
    | Pmod_constraint (mexp, mtyp) -> constraint_ mexp mtyp
    | Pmod_unpack _expr -> todo ~what:"unpack " ()
    | Pmod_extension _ext ->
      print_endline "extension";
      todo ~what:"extension " ()

  and conv (mexp : Parsetree.module_expr) =
    mk ~loc:mexp.pmod_loc ~attrs:mexp.pmod_attributes mexp.pmod_desc

  and constraint_ ?loc:_ ?attrs:_ mexp mtyp =
    let mexp_syn = conv mexp in
    let mtyp_syn = Mty.conv mtyp in
    Syntax_builder.constraint_ mexp_syn mtyp_syn
end

and Mty : sig
  type module_type = Syntax.t

  val mk :
       ?loc:loc
    -> ?attrs:Parsetree.attributes
    -> Parsetree.module_type_desc
    -> module_type

  (* val attr : Parsetree.module_type -> Parsetree.attribute -> module_type
     val ident : ?loc:loc -> ?attrs:Parsetree.attributes -> lid -> module_type
     val alias : ?loc:loc -> ?attrs:Parsetree.attributes -> lid -> module_type *)

  val signature :
       ?loc:loc
    -> ?attrs:Parsetree.attributes
    -> Parsetree.signature
    -> module_type

  (* val functor_ :
          ?loc:loc
       -> ?attrs:Parsetree.attributes
       -> Parsetree.functor_parameter
       -> Parsetree.module_type
       -> module_type

     val with_ :
          ?loc:loc
       -> ?attrs:Parsetree.attributes
       -> Parsetree.module_type
       -> Parsetree.with_constraint list
       -> module_type

     val typeof_ :
          ?loc:loc
       -> ?attrs:Parsetree.attributes
       -> Parsetree.module_expr
       -> module_type

     val extension :
          ?loc:loc
       -> ?attrs:Parsetree.attributes
       -> Parsetree.extension
       -> module_type *)

  val conv : Parsetree.module_type -> Syntax.t
end = struct
  type module_type = Syntax.t

  let todo what = Syntax.id ("$MOD: " ^ what)

  let rec mk ?loc:_ ?attrs:_ (mtyp_desc : Parsetree.module_type_desc) =
    match mtyp_desc with
    | Pmty_ident { txt = id; _ } -> Syntax.Id id
    | Pmty_signature s -> signature s
    | Pmty_functor (args, mtyp) -> todo "functor"
    | Pmty_with (mtyp, withc) -> todo "with"
    | Pmty_typeof mexp -> todo "typeof"
    | Pmty_extension ext -> todo "extension"
    | Pmty_alias { txt = id; _ } -> Syntax.Id id

  and signature ?loc:_ ?attrs:_ (s : Parsetree.signature) =
    Syntax_builder.block (List.map Sig.conv s)

  let conv (mtyp : Parsetree.module_type) =
    mk ~loc:mtyp.pmty_loc ~attrs:mtyp.pmty_attributes mtyp.pmty_desc
end

and Sig : sig
  type signature_item = Syntax.t

  val mk : ?loc:loc -> Parsetree.signature_item_desc -> signature_item
  val conv : Parsetree.signature_item -> signature_item
  (*
      val value : ?loc:loc -> Parsetree.value_description -> signature_item

      val type_ :
           ?loc:loc
        -> Asttypes.rec_flag
        -> Parsetree.type_declaration list
        -> signature_item

      val type_subst : ?loc:loc -> Parsetree.type_declaration list -> signature_item
      val type_extension : ?loc:loc -> Parsetree.type_extension -> signature_item
      val exception_ : ?loc:loc -> Parsetree.type_exception -> signature_item
      val module_ : ?loc:loc -> Parsetree.module_declaration -> signature_item
      val mod_subst : ?loc:loc -> Parsetree.module_substitution -> signature_item

      val rec_module :
        ?loc:loc -> Parsetree.module_declaration list -> signature_item

      val modtype : ?loc:loc -> Parsetree.module_type_declaration -> signature_item

      val modtype_subst :
        ?loc:loc -> Parsetree.module_type_declaration -> signature_item

      val open_ : ?loc:loc -> Parsetree.open_description -> signature_item
      val include_ : ?loc:loc -> Parsetree.include_description -> signature_item
      val class_ : ?loc:loc -> Parsetree.class_description list -> signature_item

      val class_type :
        ?loc:loc -> Parsetree.class_type_declaration list -> signature_item

      val extension :
           ?loc:loc
        -> ?attrs:Parsetree.attributes
        -> Parsetree.extension
        -> signature_item

      val attribute : ?loc:loc -> Parsetree.attribute -> signature_item
      val text : Docstrings.text -> signature_item list *)
end = struct
  type signature_item = Syntax.t

  let todo what = Syntax.id ("$MOD: " ^ what)

  let rec mk ?loc:_ (sig_item_desc : Parsetree.signature_item_desc) =
    match sig_item_desc with
    | Psig_value val_desc -> Val.conv val_desc
    | Psig_type (rec_flag, type_declaration_list) -> todo "type"
    | Psig_typesubst type_declaration_list -> todo "typesubst"
    | Psig_typext type_extension -> todo "typext"
    | Psig_exception type_exception -> todo "exception"
    | Psig_module module_declaration -> todo "module"
    | Psig_modsubst module_substitution -> todo "modsubst"
    | Psig_recmodule module_declaration_list -> todo "recmodule"
    | Psig_modtype module_type_declaration -> todo "modtype"
    | Psig_modtypesubst module_type_declaration -> todo "modtypesubst"
    | Psig_open open_description -> todo "open"
    | Psig_include include_description -> todo "include"
    | Psig_class class_description_list -> todo "class"
    | Psig_class_type class_type_declaration_list -> todo "class_type"
    | Psig_attribute attribute -> todo "attribute"
    | Psig_extension (extension, attributes) -> todo "extension"

  let conv (sig_item : Parsetree.signature_item) =
    mk ~loc:sig_item.psig_loc sig_item.psig_desc
end

let structure str = Syntax_builder.block (List.map Str.conv str)
let signature = Mty.signature
