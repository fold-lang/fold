open Prelude
module Fl = Fold_ast.Cons

type fl = Shaper.syntax
type loc = Location.t
type docs = Docstrings.docs
type text = Docstrings.text
type attrs = Parsetree.attribute list
type const = Parsetree.constant
type ident = Longident.t

module Ml = struct
  include Ast_helper

  let unit_lid = Longident.Lident "()"
  let nil_lid = Longident.Lident "[]"
  let cons_lid = Longident.Lident "::"
end

let conv_const (const : Shaper.const) =
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
    (* -- let_in_unit -- *)
    | Shape ("let", [ Seq (Some ",", vbl) ]) -> let_in_unit vbl
    | Shape ("let", [ vb ]) -> let_in_unit [ vb ]
    (* --- ident --- *)
    | Ident (Lower x) -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    | Sym x -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    (* --- const --- *)
    | Const const -> Ml.Exp.constant (conv_const const)
    (* --- fn --- *)
    | Shape ("fn", [ Shape ("->", [ Seq (None, args); body ]) ]) -> fn args body
    | Shape ("fn", [ Shape ("->", [ arg; body ]) ]) -> fn [ arg ] body
    | Shape ("fn", [ Scope ("{", Shape ("->", [ arg; body ]), "}") ]) ->
      fn [ arg ] body
    (* --- fn_match --- *)
    | Shape ("fn", [ Scope ("{", Seq (Some ",", cases), "}") ]) ->
      fn_match cases
    (* --- if_then_else --- *)
    | Shape ("if", [ a; b; c ]) -> if_then_else a b c
    (* --- if_then --- *)
    | Shape ("if", [ a; b ]) -> if_then a b
    (* match exp cases *)
    | Shape
        ( "match"
        , [ Seq (None, [ exp_fl; Scope ("{", Seq (Some ",", cases_fl), "}") ]) ]
        ) -> match' exp_fl cases_fl
    (* --- quote --- *)
    | Shape ("quote", [ x ]) -> quote x
    (* --- construct --- *)
    | Ident (Upper id) -> construct (Longident.Lident id) []
    | Scope ("(", Seq (None, []), ")") -> construct (Longident.Lident "()") []
    | Scope ("{", Seq (None, []), "}") -> construct (Longident.Lident "()") []
    | Seq (None, Ident (Upper c) :: args) -> construct (Longident.Lident c) args
    (* --- apply --- *)
    | Seq (None, f :: args) -> apply f args
    (* a; b; b *)
    | Seq (Some ";", xs) -> block xs
    (* Err: a, b *)
    | Seq (Some sep, _) -> todo ("seq_" ^ sep)
    (* Tuple *)
    | Scope ("(", Seq (Some ",", []), ")") -> todo "tuple"
    (* --- group --- *)
    | Scope ("(", fl, ")") -> conv fl
    | Scope ("{", x, "}") -> conv x
    (* --- List --- *)
    (* [a, b & tl] *)
    | Scope ("[", Shape ("&", [ Seq (Some ",", items); tl ]), "]") ->
      list ?spread:(Some tl) items
    (* [a & tl] *)
    | Scope ("[", Shape ("&", [ a; tl ]), "]") -> list ?spread:(Some tl) [ a ]
    (* list: [a, b, c] *)
    | Scope ("[", Seq (Some ",", items), "]") -> list ?spread:None items
    (* list: [] *)
    | Scope ("[", Seq (None, []), "]") -> list ?spread:None []
    (* list: [a] *)
    | Scope ("[", item, "]") -> list ?spread:None [ item ]
    (* Err: other forms *)
    | _ ->
      Fmt.epr "---@.%a@.---@." Shaper.dump fl;
      failwith "eval: unknown syntax"

  and fn (args_fl : fl list) (body_fl : fl) =
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
          Fmt.epr "@.args=%a@.body=%a@.@."
            (Fmt.Dump.list Shaper.dump)
            args_fl Shaper.dump body_fl;
          assert false
      )
      body_ml args_fl

  and tuple items = Ml.Exp.tuple (List.map conv items)

  and case (fl : fl) =
    match fl with
    | Shape ("->", [ pat_fl; exp_fl ]) ->
      let pat_ml = Pat.conv pat_fl in
      let exp_ml = Exp.conv exp_fl in
      Ml.Exp.case ?guard:None pat_ml exp_ml
    | _ -> assert false

  and fn_match (cases_fl : fl list) =
    let cases_ml = List.map case cases_fl in
    Ml.Exp.function_ cases_ml

  and apply f_fl args_fl =
    let f_ml = conv f_fl in
    let args_ml =
      List.map (fun arg_fl -> (Asttypes.Nolabel, conv arg_fl)) args_fl
    in
    Ml.Exp.apply f_ml args_ml

  and match' exp_fl cases_fl =
    let exp_ml = conv exp_fl in
    let cases_ml = List.map case cases_fl in
    Ml.Exp.match_ exp_ml cases_ml

  and let_in_unit vbl_fl =
    let vbl_ml = List.map Vb.conv vbl_fl in
    Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml unit

  and block (xs : fl list) =
    match xs with
    | [] -> unit
    | [ x ] -> conv x
    | Shape ("let", [ Seq (Some ",", vbl) ]) :: xs ->
      let vbl_ml = List.map Vb.conv vbl in
      let body_ml = block xs in
      Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml
    | Shape ("let", [ vb ]) :: xs ->
      let vbl_ml = [ Vb.conv vb ] in
      let body_ml = block xs in
      Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml
    | Shape ("open", [ mexp ]) :: items ->
      let odcl = Odcl.conv mexp in
      Ml.Exp.open_ odcl (block items)
    | item :: items -> Ml.Exp.sequence (conv item) (block items)

  and construct id args_fl =
    match args_fl with
    | [] -> Ml.Exp.construct (Location.mknoloc id) None
    | [ arg_fl ] ->
      let arg_ml = conv arg_fl in
      Ml.Exp.construct (Location.mknoloc id) (Some arg_ml)
    | _ ->
      (* [TODO] Add explicity arity ext like Reason. *)
      let arg_ml = Ml.Exp.tuple (List.map conv args_fl) in
      Ml.Exp.construct (Location.mknoloc id) (Some arg_ml)

  and list ?spread:tl xs =
    match xs with
    | [] -> Option.fold ~some:conv ~none:nil tl
    | x_fl :: xs_fl ->
      let x_ml = conv x_fl in
      let xs_ml = list ?spread:tl xs_fl in
      cons x_ml xs_ml

  and if_then_else cond_fl then_fl else_fl =
    let cond_ml = conv cond_fl in
    let then_ml = conv then_fl in
    let else_ml = conv else_fl in
    Ml.Exp.ifthenelse cond_ml then_ml (Some else_ml)

  and if_then cond_fl then_fl =
    let cond_ml = conv cond_fl in
    let then_ml = conv then_fl in
    Ml.Exp.ifthenelse cond_ml then_ml None

  and quote (fl : fl) : Parsetree.expression = conv (Fold_meta.quote fl)
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
    | Shape ("=", [ pat_fl; exp_fl ]) ->
      let pat_ml = Pat.conv pat_fl in
      let exp_ml = Exp.conv exp_fl in
      Ml.Vb.mk pat_ml exp_ml
    | _ ->
      Fmt.epr "not a vb: %a@." Shaper.dump fl;
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
    | Shape ("=", [ Ident (Upper m_name); mexp_fl ]) ->
      let m_name = if String.equal m_name "_" then None else Some m_name in
      let m_name = Location.mknoloc m_name in
      let mexp_ml = Mod.conv mexp_fl in
      Ml.Mb.mk m_name mexp_ml
    | _ ->
      Fmt.epr "not a vb: %a@." Shaper.dump fl;
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
      Fmt.epr "todo: Mod:@.%a@." Shaper.dump fl;
      assert false
end

and Str : sig
  val conv : fl -> Parsetree.structure_item
end = struct
  let todo what =
    Ml.Str.type_ Asttypes.Nonrecursive
      [ Ml.Type.mk (Location.mknoloc ("__str_" ^ what)) ]

  let rec conv (fl : fl) =
    match fl with
    | Shape ("=", [ _lhs; _rhs ]) -> value [ fl ]
    | Seq (Some ",", vbl) -> value vbl
    | Shape ("let", [ Seq (Some ",", vbl) ]) -> value vbl
    | Shape ("let", [ vb ]) -> value [ vb ]
    | Shape ("module", [ mb ]) -> module' mb
    | Shape ("open", [ mexp ]) -> Ml.Str.open_ (Odcl.conv mexp)
    | Ident _ | Const _ | Sym _ | Seq (None, _ :: _) | Scope _ -> eval fl
    | Shape (kwd, _xs) -> todo ("form_" ^ kwd)
    | Seq (None, _) -> failwith "invalid seq/aply"
    | Seq (Some sep, _) -> todo ("seq_" ^ sep)

  and eval fl = Ml.Str.eval (Exp.conv fl)

  and value vbl_fl =
    let vbl_ml = List.map Vb.conv vbl_fl in
    Ml.Str.value Asttypes.Nonrecursive vbl_ml

  and module' mb_fl =
    let mb_ml = Mb.conv mb_fl in
    Ml.Str.module_ mb_ml

  and open' mexp = Ml.Str.open_ (Odcl.conv mexp)
end

let structure (fl : fl) : Parsetree.structure =
  match fl with
  | Seq (Some ";", items) -> List.map Str.conv items
  | _ -> [ Str.conv fl ]

module Pp = struct
  let structure : Format.formatter -> Parsetree.structure -> unit =
    Pprintast.structure
end
