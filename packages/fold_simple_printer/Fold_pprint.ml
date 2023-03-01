module Syntax = Fold_syntax.Syntax
module P = PPrint

let ( <| ) = ( @@ )

let foldli (f : int -> 'b -> 'a -> 'b) (accu : 'b) (xs : 'a list) : 'b =
  let r = ref 0 in
  List.fold_left
    (fun accu x ->
      let i = !r in
      r := i + 1;
      f i accu x)
    accu xs

let ( ^^ ) = PPrint.( ^^ )
let ( !^ ) = PPrint.( !^ )
let ( ^/^ ) = PPrint.( ^/^ )
let parens_on b doc = if b then P.parens doc else doc
let on b doc = if b then doc else P.empty

module Syntax_formatter = struct
  let fmt_id id = P.string (Fmt.str "%a" Syntax.pp_id id)
  let fmt_const const = P.string (Fmt.str "%a" Syntax.pp_const const)

  let rec fmt_syntax ?(enclose = true) (syn : Syntax.t) =
    match syn with
    | Id id -> fmt_id id
    | Const const -> fmt_const const
    | Apply (f, args) -> fmt_apply ~enclose f args
    | Form items -> fmt_form items
    | Block items -> fmt_block items
    | Record (r0, fields) -> fmt_record r0 fields
    | Tuple items -> fmt_seq P.parens items
    | Array items -> fmt_seq P.braces items
    | List (items, tl) -> fmt_list items tl
    | Binding (lval, ((Block _ | Record _) as rval)) ->
      fmt_binding_bracket lval (fmt_syntax rval)
    | Binding (lval, rval) -> fmt_binding lval rval
    | Fn (args, ((Block _ | Record _) as body)) ->
      fmt_fn_bracket ~enclose args body
    | Fn (args, body) -> fmt_fn ~enclose args body
    | Labeled (l, optional, value) -> fmt_labeled l optional value
    | Field (r, l) -> fmt_field r l
    | Or items -> fmt_or items
    | Arrow (a, b) -> fmt_arrow a b
    | Constraint (a, b) -> fmt_constraint ~enclose a b
    | _ -> P.string "$FMT"

  and fmt (syn : Syntax.t) =
    match syn with
    | Block items -> root_block items
    | _ -> fmt_syntax ~enclose:false syn

  and root_block items = P.flow_map (P.twice P.hardline) fmt_syntax items

  and fmt_binding lval rval =
    match (lval, rval) with
    | Syntax.Id (Longident.Lident lid), Syntax.Id (Longident.Lident rid)
      when String.equal lid rid -> P.string "~" ^^ P.string lid
    | _ ->
      P.group
        (fmt_syntax ~enclose:false lval
        ^^ P.blank 1
        ^^ P.string "="
        ^^ P.nest 2 (P.break 1 ^^ (fmt_syntax ~enclose:false) rval))

  and fmt_arrow a b =
    P.group
      (fmt_syntax ~enclose:false a
      ^^ P.blank 1
      ^^ P.string "->"
      ^^ P.nest 2 (P.break 1 ^^ (fmt_syntax ~enclose:false) b))

  and fmt_constraint ~enclose a b =
    P.group
      (parens_on enclose
         (fmt_syntax ~enclose:false a
         ^^ P.string " : "
         ^^ P.nest 2 ((fmt_syntax ~enclose:false) b)))

  and fmt_block items =
    if items = [] then P.braces P.empty
    else
      P.group
        (P.braces
           (P.nest 2
              (P.break 1
              ^^ P.flow_map (P.semi ^^ P.hardline)
                   (fmt_syntax ~enclose:false)
                   items
              ^^ P.semi)
           ^^ P.break 1))

  and fmt_seq brackets items =
    if items = [] then brackets P.empty
    else
      P.group
        (brackets
           (P.nest 2
              (P.flow_map
                 (P.comma ^^ P.break 1)
                 (fmt_syntax ~enclose:false)
                 items)))

  and fmt_list items tl =
    if items = [] then
      match tl with
      | Some tl -> fmt_syntax ~enclose:false tl
      | None -> P.brackets P.empty
    else
      let tl_doc =
        match tl with
        | None -> P.empty
        | Some tl -> P.string ", .." ^^ fmt_syntax ~enclose:false tl
      in
      P.group
        (P.brackets
           (P.nest 2
              (P.break 1
              ^^ P.flow_map
                   (P.comma ^^ P.break 1)
                   (fmt_syntax ~enclose:false)
                   items)
           ^^ tl_doc
           ^^ P.break 1))

  and fmt_binding_bracket lval rval =
    P.group (fmt_syntax lval ^^ P.space ^^ P.string "=" ^^ P.space ^^ rval)

  and fmt_juxt items = P.group (P.flow_map (P.break 1) fmt_syntax items)

  and fmt_apply ~enclose f args =
    match (f, args) with
    | _, [] -> fmt_syntax f
    (* Infix *)
    | Syntax.Id (Longident.Lident op), [ arg_1; arg_2 ]
      when Fold_syntax.Operators.is_infix op ->
      let doc_1 = fmt_syntax ~enclose:false arg_1 in
      let doc_2 = fmt_syntax ~enclose:false arg_2 in
      parens_on enclose (P.group (doc_1 ^/^ P.string op ^^ P.space ^^ doc_2))
    (* Prefix *)
    | Syntax.Id (Longident.Lident op), [ arg ]
      when Fold_syntax.Operators.is_prefix op ->
      P.string op ^^ fmt_syntax ~enclose:false arg
    | _ ->
      parens_on enclose <| P.group (P.prefix 2 1 (fmt_syntax f) (fmt_juxt args))

  and fmt_form_ items =
    P.group (P.flow_map (P.break 1) (fmt_syntax ~enclose:false) items)

  and flow_map_sep mk_sep mk_doc items =
    foldli
      (fun i acc x ->
        let sep = mk_sep x in
        if i = 0 then mk_doc x else acc ^^ P.group (sep ^^ mk_doc x))
      P.empty items

  and fmt_form items =
    match items with
    | [] -> P.empty
    (* let rec *)
    | Syntax.(Id (Lident "let") :: Id (Lident "rec") :: bindings) ->
      P.group
        (!^"let rec"
        ^^ P.concat_map
             (function
               | x ->
                 (if Syntax.is_binding x then P.space else P.break 1)
                 ^^ fmt_syntax x)
             bindings)
    (* let *)
    | Syntax.(Id (Lident "let") :: bindings) ->
      P.group
        (!^"let"
        ^^ P.concat_map
             (function
               | x ->
                 (if Syntax.is_binding x then P.space else P.break 1)
                 ^^ fmt_syntax x)
             bindings)
    (* val rec *)
    | Syntax.(Id (Lident "val") :: Id (Lident "rec") :: bindings) ->
      P.group
        (!^"val rec"
        ^^ P.concat_map
             (function
               | x ->
                 (if Syntax.is_binding x then P.space else P.twice P.hardline)
                 ^^ fmt_syntax x)
             bindings)
    (* val *)
    | Syntax.(Id (Lident "val") :: bindings) ->
      P.group
        (!^"val"
        ^^ P.concat_map
             (function
               | x ->
                 (if Syntax.is_binding x then P.space else P.twice P.hardline)
                 ^^ fmt_syntax x)
             bindings)
    | Syntax.[ Id (Lident "if"); cond; Id (Lident "then"); if_true ] ->
      P.group
        (!^"if"
        ^^ P.space
        ^^ fmt_syntax ~enclose:false cond
        ^^ P.space
        ^^ !^"then"
        ^^ begin
             if Syntax.is_block if_true then
               P.space ^^ fmt_syntax ~enclose:false if_true
             else P.nest 2 (P.break 1 ^^ fmt_syntax ~enclose:false if_true)
           end)
    | Syntax.
        [ Id (Lident "if")
        ; cond
        ; Id (Lident "then")
        ; if_true
        ; Id (Lident "else")
        ; if_false
        ] ->
      P.group
        (!^"if"
        ^^ P.space
        ^^ fmt_syntax ~enclose:false cond
        ^^ P.space
        ^^ !^"then"
        ^^ begin
             if Syntax.is_block if_true then
               P.space ^^ fmt_syntax ~enclose:false if_true
             else P.nest 2 (P.break 1 ^^ fmt_syntax ~enclose:false if_true)
           end
        ^^ P.break 1
        ^^ !^"else"
        ^^ begin
             if Syntax.is_block if_false then
               P.space ^^ fmt_syntax ~enclose:false if_false
             else P.nest 2 (P.break 1 ^^ fmt_syntax ~enclose:false if_false)
           end)
    | _ -> P.group (P.flow_map P.space (fmt_syntax ~enclose:false) items)

  and fmt_fn ~enclose args body =
    P.group
      (parens_on enclose
         (P.string "fn"
         ^^ P.space
         ^^ P.flow_map (P.break 1) fmt_syntax args
         ^^ P.space
         ^^ P.string "->"
         ^^ P.nest 2 (P.break 1 ^^ fmt_syntax ~enclose:false body)))

  and fmt_fn_bracket ~enclose args body =
    P.group
      (parens_on enclose
         (P.string "fn"
         ^^ P.space
         ^^ P.flow_map (P.break 1) fmt_syntax args
         ^^ P.space
         ^^ P.string "->"
         ^^ P.space
         ^^ fmt_syntax body))

  and fmt_record r0 fields =
    let fields_doc = P.flow_map (P.comma ^^ P.hardline) fmt_syntax fields in
    let r0_doc =
      match r0 with
      | Some r0 -> P.comma ^/^ P.string ".." ^^ fmt_syntax r0
      | None -> P.empty
    in
    P.group
      (P.lbrace
      ^^ P.nest 2 (P.break 1 ^^ fields_doc ^^ r0_doc)
      ^^ P.break 1
      ^^ P.rbrace)

  and fmt_labeled l optional value =
    let map_to_optional_id (syn : Syntax.t) : Syntax.t =
      match syn with
      | Binding (Id (Lident id), rval) -> Binding (Id (Lident (id ^ "?")), rval)
      | Constraint (Id (Lident id), rval) ->
        Constraint (Id (Lident (id ^ "?")), rval)
      | Binding (Constraint (Id (Lident id), typ), rval) ->
        Binding (Constraint (Id (Lident (id ^ "?")), typ), rval)
      | _ -> syn
    in
    let enclose = Syntax.is_binding value in
    match value with
    | Id (Lident id) when String.equal id l ->
      if optional then P.string "~" ^^ P.string l ^^ P.string "?"
      else P.string "~" ^^ P.string l
    | Binding ((Id (Lident id) | Constraint (Id (Lident id), _)), _)
    | Constraint (Id (Lident id), _) ->
      if String.equal id l then
        let constraint' =
          if optional then map_to_optional_id value else value
        in
        P.string "~" ^^ parens_on enclose (fmt_syntax ~enclose:true constraint')
      else
        P.string "~"
        ^^ P.string l
        ^^ (if optional then P.string "?:" else P.string ":")
        ^^ parens_on enclose (fmt_syntax value)
    | _ ->
      P.string "~"
      ^^ P.string l
      ^^ P.string (if optional then "?:" else ":")
      ^^ parens_on enclose (fmt_syntax value)

  and fmt_field r l = fmt_syntax r ^^ P.string "." ^^ fmt_id l

  and fmt_or items =
    if items = [] then P.braces P.empty
    else
      P.group
        (P.braces
           (P.break 1
           ^^ P.string "| "
           ^^ P.flow_map
                (P.hardline ^^ P.string "| ")
                (fmt_syntax ~enclose:false)
                items
           ^^ P.break 1))
end

let print syntax =
  let doc = Syntax_formatter.fmt syntax in
  P.ToChannel.pretty 0.8 80 stdout doc;
  print_newline ()
