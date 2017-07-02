open Pure

module ID = struct
  type lowercase = string
  type capitalized = string
end

module Type = struct
  type t
  let constructor cap_id = undefined ()
  let var low_id = undefined ()
  let tuple l = undefined ()
end

module Expression = struct
  type t = Parsetree.expression

  let token tok =
    let open Parsetree in
    let desc =
      match tok with
      | `Bool   x -> Pexp_construct (Location.mknoloc (Longident.Lident (string_of_bool x)), None)
      | `Char   x -> Pexp_constant (Pconst_char x)
      | `Float  x -> Pexp_constant (Pconst_float (string_of_int x, None))
      | `Int    x -> Pexp_constant (Pconst_integer (string_of_int x, None))
      | `String x -> Pexp_constant (Pconst_string (x, None))
      | `Symbol x -> Pexp_ident (Location.mknoloc (Longident.Lident x))
    in
      { pexp_desc = desc; pexp_loc = Location.none; pexp_attributes = [] }

  let let' pattern expr body =
    let value_binding =
      Parsetree.{ pvb_pat = pattern;
                  pvb_expr = expr;
                  pvb_attributes = [];
                  pvb_loc = Location.none } in
    let desc = Parsetree.Pexp_let (Asttypes.Nonrecursive, [value_binding], body) in
    Parsetree.{ pexp_desc = desc; pexp_loc = Location.none; pexp_attributes = [] }
end

module Pattern = struct
  type t = Parsetree.pattern

  let symbol x =
    let desc = Parsetree.Ppat_var (Location.mknoloc x) in
    Parsetree.{ ppat_desc = desc; ppat_loc = Location.none; ppat_attributes = [] }

  let token tok =
    let open Parsetree in
    let desc =
      match tok with
      | `Bool   x -> Ppat_construct (Location.mknoloc (Longident.Lident (string_of_bool x)), None)
      | `Char   x -> Ppat_constant (Pconst_char x)
      | `Float  x -> Ppat_constant (Pconst_float (string_of_int x, None))
      | `Int    x -> Ppat_constant (Pconst_integer (string_of_int x, None))
      | `String x -> Ppat_constant (Pconst_string (x, None))
      | `Symbol x -> Ppat_var (Location.mknoloc x)
    in
      { ppat_desc = desc; ppat_loc = Location.none; ppat_attributes = [] }
end

module Statement = struct
  type t

  let val' pattern expression = undefined ()

  let def low_id pattern expression = undefined ()

  (* `type (name <- Identifier.capitalized) (parameters <- Identifier.lowercase* )
   *   `= (type <- Type.t) *)
  let type' cap_id params body = undefined ()
end


