open Local

module Name = struct
  type t = Longident.t

  let id x = Longident.Lident x
  let dot t s = Longident.Ldot (t, s)
  let apply a b = Longident.Lapply (a, b)
end

module Type = struct
  type t
  let constructor _cap_id = undefined ()
  let var _low_id = undefined ()
  let tuple _l = undefined ()
end

module Expression = struct

  type t = Parsetree.expression


  let append x y =
    let open Parsetree in
    let desc = match x with
      | { pexp_desc = Pexp_apply (f, xs); _ } -> Pexp_apply (f, List.append xs [y])
      | atom -> Pexp_apply (atom, [y]) in
    Parsetree.{ pexp_desc = desc; pexp_loc = Location.none; pexp_attributes = [] }


  let token (tok : Lex.Token.t) =
    let open Parsetree in
    let desc =
      match tok with
      | `Bool   x -> Pexp_construct (Location.mknoloc (Longident.Lident (string_of_bool x)), None)
      | `Char   x -> Pexp_constant (Pconst_char x)
      | `Float  x -> Pexp_constant (Pconst_float (string_of_float x, None))
      | `Int    x -> Pexp_constant (Pconst_integer (string_of_int x, None))
      | `String x -> Pexp_constant (Pconst_string (x, None))
      | `Symbol x -> Pexp_ident (Location.mknoloc (Longident.Lident x))
    in
      { pexp_desc = desc; pexp_loc = Location.none; pexp_attributes = [] }

  let apply f xs =
    let open Parsetree in
    let desc = Parsetree.Pexp_apply
        (f, (List.map (fun x -> (Asttypes.Nolabel, x)) xs)) in
    { pexp_desc = desc; pexp_loc = Location.none; pexp_attributes = [] }


  let tuple items =
    let desc = Parsetree.Pexp_tuple items in
    Parsetree.{ pexp_desc = desc; pexp_loc = Location.none; pexp_attributes = [] }


  let let' bindings body =
    let bindings' =
      List.map (fun (pattern, expr) ->
          Parsetree.{ pvb_pat = pattern;
                      pvb_expr = expr;
                      pvb_attributes = [];
                      pvb_loc = Location.none })
        bindings
    in
    let desc = Parsetree.Pexp_let (Asttypes.Nonrecursive, bindings', body) in
    Parsetree.{ pexp_desc = desc; pexp_loc = Location.none; pexp_attributes = [] }


  let lambda ?label ?default arg body =
    let arg_label =
      match label with
      | Some x -> Asttypes.Labelled x
      | None -> Asttypes.Nolabel in
    let open Parsetree in
    { pexp_desc = Pexp_fun (arg_label, default, arg, body);
      pexp_loc = Location.none;
      pexp_attributes = [] }
end

module Pattern = struct
  type t = Parsetree.pattern


  let symbol x =
    let desc = Parsetree.Ppat_var (Location.mknoloc x) in
    Parsetree.{ ppat_desc = desc; ppat_loc = Location.none; ppat_attributes = [] }


  let constructor name (args : Parsetree.pattern list) =
    let open Parsetree in
    let args' =
      match args with
      | [] -> None
      | [x] -> Some x
      | xs -> Some { ppat_desc = Ppat_tuple xs; ppat_loc = Location.none; ppat_attributes = []} in
    let desc = Ppat_construct (Location.mknoloc name, args') in
    { ppat_desc = desc; ppat_loc = Location.none; ppat_attributes = [] }


  let tuple items =
    let desc = Parsetree.Ppat_tuple items in
    Parsetree.{ ppat_desc = desc; ppat_loc = Location.none; ppat_attributes = [] }


  let token (tok : Lex.Token.t) =
    let open Parsetree in
    let desc =
      match tok with
      | `Bool   x -> Ppat_construct (Location.mknoloc (Longident.Lident (string_of_bool x)), None)
      | `Char   x -> Ppat_constant (Pconst_char x)
      | `Float  x -> Ppat_constant (Pconst_float (string_of_float x, None))
      | `Int    x -> Ppat_constant (Pconst_integer (string_of_int x, None))
      | `String x -> Ppat_constant (Pconst_string (x, None))
      | `Symbol x -> Ppat_var (Location.mknoloc x)
    in
      { ppat_desc = desc; ppat_loc = Location.none; ppat_attributes = [] }
end

module Statement = struct
  type t = Parsetree.structure_item

  let val' pattern expression =
    let open Parsetree in
    let value_binding =
      { pvb_pat = pattern
      ; pvb_expr = expression
      ; pvb_attributes = []
      ; pvb_loc = Location.none
      } in
    let desc = Pstr_value (Asttypes.Nonrecursive, [value_binding]) in
    { pstr_desc = desc; pstr_loc = Location.none }


  let def pattern expression =
    let open Parsetree in
    let value_binding =
      { pvb_pat = pattern
      ; pvb_expr = expression
      ; pvb_attributes = []
      ; pvb_loc = Location.none
      } in
    let desc = Pstr_value (Asttypes.Recursive, [value_binding]) in
    { pstr_desc = desc; pstr_loc = Location.none }

  (* `type (name <- Identifier.capitalized) (parameters <- Identifier.lowercase* )
   *   `= (type <- Type.t) *)
  let type' _cap_id _params _body = undefined ()
end

module Module = struct
  type t = Statement.t list

  let make xs = xs
end
