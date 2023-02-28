module Syntax = Fold_syntax.Syntax
module P = PPrint

let ( <| ) = ( @@ )

let flowing_list f xs =
  let open PPrint in
  group
    (lbracket
    ^^ space
    ^^ nest 2 (flow_map (semi ^^ break 1) f xs)
    ^^ space
    ^^ rbracket)

let seq1 opening separator closing =
  let open PPrint in
  surround_separate 2 0 (opening ^^ closing) opening
    (separator ^^ break 2)
    closing

let ( ^^ ) = PPrint.( ^^ )
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
    | List items -> fmt_seq P.brackets items
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
    | _ -> P.string "TODO"

  and fmt (syn : Syntax.t) =
    match syn with
    | Block items -> root_block items
    | _ -> fmt_syntax ~enclose:false syn

  and root_block items =
    P.flow_map (P.semi ^^ P.twice P.hardline) fmt_syntax items ^^ P.semi

  and fmt_binding lval rval =
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

  and fmt_binding_bracket lval rval =
    P.group (fmt_syntax lval ^^ P.space ^^ P.string "=" ^^ P.space ^^ rval)

  and fmt_juxt items = P.group (P.flow_map (P.break 1) fmt_syntax items)

  and fmt_apply ~enclose f args =
    match args with
    | [] -> fmt_syntax f
    | _ ->
      parens_on enclose <| P.group (P.prefix 2 1 (fmt_syntax f) (fmt_juxt args))

  and fmt_form items = P.group (P.flow_map (P.blank 1) fmt_syntax items)

  and fmt_fn ~enclose args body =
    P.group
      (parens_on enclose
         (P.string "fn"
         ^^ P.space
         ^^ P.flow_map (P.break 1) fmt_syntax args
         ^^ P.space
         ^^ P.string "->"
         ^^ P.nest 2 (P.break 1 ^^ fmt_syntax body)))

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
    let fields_doc = List.map fmt_syntax fields in
    let items_doc =
      match r0 with
      | Some r0 -> (P.string ".." ^^ fmt_syntax r0) :: fields_doc
      | None -> fields_doc
    in
    P.group
      (P.lbrace
      ^^ P.nest 2
           (P.break 1 ^^ P.flow (P.comma ^^ P.hardline) items_doc ^^ P.comma)
      ^^ P.break 1
      ^^ P.rbrace)

  and fmt_labeled l optional value =
    let prefix = P.string "~" ^^ P.string l in
    let enclose = Syntax.is_binding value in
    match value with
    | Id (Lident id) when String.equal id l ->
      if optional then prefix ^^ P.string "?" else prefix
    | _ ->
      prefix
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
