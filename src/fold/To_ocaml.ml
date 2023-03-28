module Fl = Shaper

type fl = Fl.shape
type loc = Location.t
type docs = Docstrings.docs
type text = Docstrings.text
type attrs = Parsetree.attribute list

module Ml = struct
  include Migrate_parsetree.Migrate_414_500.To.Parsetree
  include Ast_helper

  let unit_lid = Longident.Lident "()"
  let nil_lid = Longident.Lident "[]"
  let cons_lid = Longident.Lident "::"
end

let conv_const (const : Fl.const) =
  match const with
  | Int x -> Ml.Const.int x
  | Float x -> Ml.Const.float (string_of_float x)
  | String x -> Ml.Const.string x
  | Char x -> Ml.Const.char x

module rec Exp : sig
  val conv : fl -> Parsetree.expression
end = struct
  let todo what =
    Ml.Exp.ident (Location.mknoloc (Longident.Lident ("__exp_" ^ what)))

  let unit = Ml.Exp.construct (Location.mknoloc Ml.unit_lid) None
  let nil = Ml.Exp.construct (Location.mknoloc Ml.nil_lid) None

  let cons x xs =
    Ml.Exp.construct
      (Location.mknoloc Ml.cons_lid)
      (Some (Ml.Exp.tuple [ x; xs ]))

  let rec conv (fl : fl) : Parsetree.expression =
    match fl with
    (* `let $vbl_fl $(; rest)*` *)
    (* Sequence and let bindings *)
    | Seq (Some ";", Form ("let", [ Seq (Some ",", vbl) ]) :: rest) ->
      let_ vbl rest
    | Seq (Some ";", Form ("let", [ vb ]) :: rest) -> let_ [ vb ] rest
    (* let a = 1 in () *)
    | Form ("let", [ Seq (Some ",", vbl) ]) -> let_ vbl []
    | Form ("let", [ vb ]) -> let_ [ vb ] []
    (* Ident *)
    | Ident (Lower x) -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    | Ident (Upper x) -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    | Const const -> Ml.Exp.constant (conv_const const)
    | Sym x -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    (* fn _ _ _ -> _ *)
    | Form ("fn", [ Form ("->", [ Seq (None, args); body ]) ]) -> fun_ args body
    (* fn _ -> _ *)
    | Form ("fn", [ Form ("->", [ arg; body ]) ]) -> fun_ [ arg ] body
    (* fn { _ -> _ } *)
    | Form ("fn", [ Scope ("{", Form ("->", [ arg; body ]), "}") ]) ->
      fun_ [ arg ] body
    (* fn { _ -> _ | _ -> _ } *)
    | Form ("fn", [ Scope ("{", Form ("|", cases), "}") ]) -> function_ cases
    (* if _ then _ else _ *)
    | Form
        ( "if"
        , [ cond_fl; Scope ("{", if_true, "}"); Scope ("{", if_false, "}") ]
        ) -> ifthenelse cond_fl if_true (Some if_false)
    | Form ("if", [ cond_fl; Scope ("{", if_true, "}") ]) ->
      ifthenelse cond_fl if_true None
    | Form ("if", _) ->
      Fmt.epr "ctx: %a@." Fl.dump fl;
      failwith "invalid if syntax"
    (* match exp cases *)
    | Form
        ( "match"
        , [ Seq (None, [ exp_fl; Scope ("{", Seq (Some ",", cases_fl), "}") ]) ]
        ) -> match_ exp_fl cases_fl
    (* `a *)
    | Form ("quote", [ x ]) -> quote x
    (* Err: other forms *)
    | Form (kwd, _xs) -> todo ("form_" ^ kwd)
    (* Construct *)
    | Seq (None, Ident (Upper id) :: args) -> construct id args
    (* Apply *)
    | Seq (None, f :: args) -> apply f args
    | Seq (None, items) ->
      Fmt.epr "ctx: %a@." (Fmt.Dump.list Fl.dump) items;
      failwith "invalid seq/aply"
    (* a; b; b *)
    | Seq (Some ";", items) -> sequence items
    (* Err: a, b *)
    | Seq (Some sep, _) -> todo ("seq_" ^ sep)
    (* Unit *)
    | Scope ("(", Seq (None, []), ")") -> unit
    (* Tuple *)
    | Scope ("(", Seq (Some ",", []), ")") -> todo "tuple"
    | Scope ("(", fl, ")") -> conv fl
    (* List *)
    | Scope ("[", Seq (None, []), "]") -> nil
    | Scope ("[", Seq (Some ",", items), "]") -> construct_list items
    | Scope ("[", item, "]") -> construct_list [ item ]
    (* { } *)
    | Scope ("{", Seq (None, []), "}") -> unit
    (* { x }
       [TODO] handle arrays, empty {}, singleton, etc. *)
    | Scope ("{", x, "}") -> conv x
    | Scope _ -> todo "scope"

  and fun_ (args_fl : fl list) (body_fl : fl) =
    let body_ml = conv body_fl in
    List.fold_left
      (fun acc arg_fl ->
        match (arg_fl : fl) with
        | Ident (Lower id) ->
          let pat = Pat.conv arg_fl in
          let label = Asttypes.Nolabel in
          let default = None in
          Ml.Exp.fun_ label default pat acc
        | _ ->
          Fmt.epr "@.args=%a@.body=%a@.@." (Fmt.Dump.list Fl.dump) args_fl
            Fl.dump body_fl;
          assert false
      )
      body_ml args_fl

  and case (fl : fl) =
    match fl with
    | Form ("->", [ pat_fl; exp_fl ]) ->
      let pat_ml = Pat.conv pat_fl in
      let exp_ml = Exp.conv exp_fl in
      Ml.Exp.case ?guard:None pat_ml exp_ml
    | _ -> assert false

  and function_ (cases_fl : fl list) =
    let cases_ml = List.map case cases_fl in
    Ml.Exp.function_ cases_ml

  and apply f_fl args_fl =
    let f_ml = conv f_fl in
    let args_ml =
      List.map (fun arg_fl -> (Asttypes.Nolabel, conv arg_fl)) args_fl
    in
    Ml.Exp.apply f_ml args_ml

  and match_ ?loc:_ ?attrs:_ exp_fl cases_fl =
    let exp_ml = conv exp_fl in
    let cases_ml = List.map case cases_fl in
    Ml.Exp.match_ exp_ml cases_ml

  and let_ vbl_fl body_fl =
    let rec unflatten items =
      match items with
      | [] -> unit
      | [ item ] -> conv item
      | Fl.Form ("let", vbl_fl) :: items' ->
        let vbl_ml = List.map Vb.conv vbl_fl in
        let body_ml = unflatten items' in
        Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml
      | Fl.Form ("open", [ mexp ]) :: items ->
        let odcl = Odcl.conv mexp in
        Ml.Exp.open_ odcl (unflatten items)
      | item :: items -> Ml.Exp.sequence (conv item) (unflatten items)
    in
    let vbl_ml = List.map Vb.conv vbl_fl in
    let body_ml = unflatten body_fl in
    Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml

  and construct ?loc:_ ?attrs:_ id args_fl =
    match args_fl with
    | [] -> Ml.Exp.construct (Location.mknoloc (Longident.Lident id)) None
    | [ arg_fl ] ->
      let arg_ml = conv arg_fl in
      Ml.Exp.construct (Location.mknoloc (Longident.Lident id)) (Some arg_ml)
    | _ ->
      (* [TODO] Add explicity arity ext like Reason. *)
      let arg_ml = Ml.Exp.tuple (List.map conv args_fl) in
      Ml.Exp.construct (Location.mknoloc (Longident.Lident id)) (Some arg_ml)

  and construct_list ?loc:_ ?attrs:_ items =
    match items with
    | [] -> nil
    | [ Form ("..", [ tl ]) ] -> conv tl
    | x_fl :: xs_fl ->
      let x_ml = conv x_fl in
      let xs_ml = construct_list xs_fl in
      cons x_ml xs_ml

  and ifthenelse ?loc:_ ?attrs:_ cond_fl then_fl else_fl =
    let cond_ml = conv cond_fl in
    let then_ml = conv then_fl in
    let else_ml = Option.map conv else_fl in
    Ml.Exp.ifthenelse cond_ml then_ml else_ml

  and sequence ?loc:_ ?attrs:_ items_fl =
    match items_fl with
    | [] -> unit
    | [ single ] -> conv single
    | first_fl :: items_fl ->
      let first_ml = conv first_fl in
      Ml.Exp.sequence first_ml (sequence items_fl)

  and quote (fl : fl) : Parsetree.expression = conv (Shaper_builder.meta fl)
end

and Odcl : sig
  val conv : fl -> Parsetree.open_declaration
end = struct
  let conv (fl : fl) =
    let popen_expr = Mod.conv fl in
    Parsetree.
      { popen_expr
      ; popen_override = Asttypes.Fresh
      ; popen_loc = Location.none
      ; popen_attributes = []
      }
end

and Pat : sig
  val conv : fl -> Parsetree.pattern
end = struct
  let todo what = Ml.Pat.var (Location.mknoloc ("__pat_" ^ what))

  type t = fl

  let conv (fl : fl) =
    match fl with
    | Ident (Lower id) -> Ml.Pat.var (Location.mknoloc id)
    (* Unit *)
    | Scope ("(", Seq (None, []), ")") ->
      Ml.Pat.construct (Location.mknoloc Ml.unit_lid) None
    | Const const -> Ml.Pat.constant (conv_const const)
    | Sym _ -> todo "sym"
    | _ -> todo "other"
end

and Vb : sig
  val conv : fl -> Parsetree.value_binding
end = struct
  let conv (fl : fl) =
    match fl with
    (* `a = b` *)
    | Fl.Form ("=", [ pat_fl; exp_fl ]) ->
      let pat_ml = Pat.conv pat_fl in
      let exp_ml = Exp.conv exp_fl in
      Ml.Vb.mk pat_ml exp_ml
    | _ ->
      Fmt.epr "not a vb: %a@." Fl.dump fl;
      Ml.Vb.mk (Ml.Pat.any ())
        (Ml.Exp.ident (Location.mknoloc (Longident.Lident "vb")))
end

(* Module bindings *)
and Mb : sig
  val conv : fl -> Parsetree.module_binding
end = struct
  let conv (fl : fl) =
    match fl with
    (* `a = b` *)
    | Form ("=", [ Ident (Upper m_name); mexp_fl ]) ->
      let m_name = if String.equal m_name "_" then None else Some m_name in
      let m_name = Location.mknoloc m_name in
      let mexp_ml = Mod.conv mexp_fl in
      Ml.Mb.mk m_name mexp_ml
    | _ ->
      Fmt.epr "not a vb: %a@." Fl.dump fl;
      assert false
end

and Mod : sig
  val conv : fl -> Parsetree.module_expr
end = struct
  let conv (fl : fl) =
    match fl with
    | Ident (Upper id) -> Ml.Mod.ident (Location.mknoloc (Longident.Lident id))
    | Scope ("{", Seq (None, []), "}") -> Ml.Mod.structure []
    | Scope ("{", Seq (Some ";", items_fl), "}") ->
      let items_ml = List.map Str.conv items_fl in
      Ml.Mod.structure items_ml
    | _ ->
      Fmt.epr "todo: Mod:@.%a@." Fl.dump fl;
      assert false
end

and Str : sig
  val conv : fl -> Ml.structure_item
end = struct
  let todo what =
    Ml.Str.type_ Asttypes.Nonrecursive
      [ Ml.Type.mk (Location.mknoloc ("__str_" ^ what)) ]

  let rec conv (fl : fl) =
    match fl with
    | Form ("let", [ Seq (Some ",", vbl) ]) -> value vbl
    | Form ("let", [ vb ]) -> value [ vb ]
    | Form ("module", [ mb ]) -> module_ mb
    | Form ("open", [ mexp ]) -> Ml.Str.open_ (Odcl.conv mexp)
    | Ident _ | Const _ | Sym _ | Seq (None, _ :: _) | Scope _ -> eval fl
    | Form (kwd, _xs) -> todo ("form_" ^ kwd)
    | Seq (None, _) -> failwith "invalid seq/aply"
    | Seq (Some sep, _) -> todo ("seq_" ^ sep)

  and eval fl = Ml.Str.eval (Exp.conv fl)

  and value vbl_fl =
    let vbl_ml = List.map Vb.conv vbl_fl in
    Ml.Str.value Asttypes.Nonrecursive vbl_ml

  and module_ mb_fl =
    let mb_ml = Mb.conv mb_fl in
    Ml.Str.module_ mb_ml
end

let structure (fl : fl) : Parsetree.structure =
  match fl with
  | Seq (Some ";", items) -> List.map Str.conv items
  | _ -> [ Str.conv fl ]

module Pp = struct
  let structure : Format.formatter -> Parsetree.structure -> unit =
    Pprintast.structure
end
