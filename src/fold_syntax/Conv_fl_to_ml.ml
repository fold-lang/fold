module Fl = Shaper.V03

type loc = Location.t
type docs = Docstrings.docs
type text = Docstrings.text
type attrs = Parsetree.attribute list

module Ml = struct
  include Migrate_parsetree.Migrate_414_500.To.Parsetree
  include Ast_helper
end

module rec Exp : sig
  val conv : Fl.syntax -> Parsetree.expression
end = struct
  let todo what =
    Ml.Exp.ident (Location.mknoloc (Longident.Lident ("__exp_" ^ what)))

  let unit_lid = Longident.Lident "()"
  let unit = Ml.Exp.construct (Location.mknoloc unit_lid) None

  let rec conv (fl : Fl.syntax) : Parsetree.expression =
    match fl with
    (* `let $vbl_fl $(; rest)*` *)
    | Seq (Some ";", Form ("let", [ Seq (Some ",", vbl) ]) :: rest) ->
      let_ vbl rest
    | Seq (Some ";", Form ("let", [ vb ]) :: rest) -> let_ [ vb ] rest
    | Ident (Lower x) -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    | Ident (Upper x) -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    | Const (Int x) -> Ml.Exp.constant (Ml.Const.int x)
    | Const (Float x) -> Ml.Exp.constant (Ml.Const.float (string_of_float x))
    | Const (String x) -> Ml.Exp.constant (Ml.Const.string x)
    | Const (Char x) -> Ml.Exp.constant (Ml.Const.char x)
    | Sym x -> Ml.Exp.ident (Location.mknoloc (Longident.Lident x))
    | Form (kwd, _xs) -> todo ("form_" ^ kwd)
    | Seq (None, f :: args) -> apply f args
    | Seq (None, _) -> failwith "invalid seq/aply"
    | Seq (Some sep, _) -> todo ("seq_" ^ sep)
    | Scope ("(", fl, ")") -> conv fl
    | Scope _ -> todo "scope"

  and apply f_fl args_fl =
    let f_ml = conv f_fl in
    let args_ml =
      List.map (fun arg_fl -> (Asttypes.Nolabel, conv arg_fl)) args_fl
    in
    Ml.Exp.apply f_ml args_ml

  and let_ vbl_fl body_fl =
    let rec unflatten items =
      match items with
      | [] -> unit
      | [ item ] -> conv item
      | Fl.Form ("let", vbl_fl) :: items' ->
        let vbl_ml = List.map Vb.conv vbl_fl in
        let body_ml = unflatten items' in
        Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml
      | Fl.Form ("open", _mod_exp) :: _items' -> assert false
      | item :: items' -> Ml.Exp.sequence (conv item) (unflatten items')
    in
    let vbl_ml = List.map Vb.conv vbl_fl in
    let body_ml = unflatten body_fl in
    Ml.Exp.let_ Asttypes.Nonrecursive vbl_ml body_ml
end

and Pat : sig
  type t = Fl.syntax

  val conv : t -> Parsetree.pattern
end = struct
  let todo what = Ml.Pat.var (Location.mknoloc ("__pat_" ^ what))

  type t = Fl.syntax

  let conv fl =
    match fl with
    | Fl.Ident (Lower id) -> Ml.Pat.var (Location.mknoloc id)
    | Fl.Const _ -> todo "const"
    | Fl.Sym _ -> todo "sym"
    | _ -> todo "other"
end

and Vb : sig
  type t = Fl.syntax

  val conv : t -> Parsetree.value_binding

  val mk :
       ?loc:loc
    -> ?attrs:attrs
    -> ?docs:docs
    -> ?text:text
    -> Fl.syntax
    -> Fl.syntax
    -> Parsetree.expression
end = struct
  type t = Fl.syntax

  let mk ?loc ?attrs ?docs ?text pat_fl exp_fl = assert false

  let conv (fl : Fl.syntax) =
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

and Str : sig
  val conv : Fl.syntax -> Ml.structure_item
end = struct
  let todo what =
    Ml.Str.type_ Asttypes.Nonrecursive
      [ Ml.Type.mk (Location.mknoloc ("__str_" ^ what)) ]

  let rec conv (fl : Fl.syntax) =
    match fl with
    | Form ("val", [ Seq (Some ",", vbl) ]) -> value vbl
    | Form ("val", [ vb ]) -> value [ vb ]
    | Ident _ | Const _ | Sym _ | Seq (None, _ :: _) -> eval fl
    | Form (kwd, _xs) -> todo ("form_" ^ kwd)
    | Seq (None, _) -> failwith "invalid seq/aply"
    | Seq (Some sep, _) -> todo ("seq_" ^ sep)
    | Scope _ -> todo "scope"

  and eval fl = Ml.Str.eval (Exp.conv fl)

  and value vbl_fl =
    let vbl_ml = List.map Vb.conv vbl_fl in
    Ml.Str.value Asttypes.Nonrecursive vbl_ml
end

let structure (fl : Fl.syntax) =
  match fl with
  | Seq (Some ";", items) -> List.map Str.conv items
  | _ -> [ Str.conv fl ]
