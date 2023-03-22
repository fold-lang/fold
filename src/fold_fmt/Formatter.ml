open Prelude
module Syntax = Fold_syntax.Syntax

module P = struct
  include PPrint

  let on b doc = if b then doc else empty

  let parens_on b doc = if b then parens doc else doc

  and flow_map_sep ?(inline = true) ~sep_after mk_doc items =
    let rec loop xs acc =
      match xs with
      | [] -> acc
      | [ x ] -> acc ^^ mk_doc x
      | x :: xs' ->
        let sep = sep_after x in
        let doc = mk_doc x in
        if inline then loop xs' (acc ^^ group (doc ^^ sep))
        else loop xs' (acc ^^ doc ^^ sep)
    in
    group (loop items PPrint.empty)
end

let check_is_blockish = function
  | Syntax.Block _ | Record _ | Tuple _ | Array _ | List _ -> true
  | _ -> false

let ( ^^ ) = PPrint.( ^^ )
let ( !^ ) = PPrint.( !^ )
let ( ^/^ ) = PPrint.( ^/^ )
let fmt_id id = P.string (Fmt.str "%a" Syntax.pp_id id)
let fmt_const const = P.string (Fmt.str "%a" Syntax.pp_const const)

let rec fmt ?(enclose = true) ?(inline = false) (syn : Syntax.t) =
  match syn with
  | Id id -> fmt_id id
  | Const const -> fmt_const const
  | Apply (f, args) -> fmt_apply ~enclose ~inline f args
  | Form [ Syntax.Id (Lident "->"); a; b ] -> fmt_arrow ~enclose a b
  | Form [ Syntax.Id (Lident ":"); v; t ] -> fmt_constraint ~enclose v t
  | Form [ Syntax.Id (Lident "="); lval; rval ] -> fmt_binding lval rval
  | Form (Syntax.Id (Lident "|") :: items) -> fmt_cases items
  | Form [ Syntax.Id (Lident "."); a; b ] -> fmt_field a b
  | Form items -> fmt_form ~enclose items
  | Block items -> fmt_block items
  | Record (fields, r0) -> fmt_record r0 fields
  | Tuple items -> fmt_seq P.parens items
  | Array items -> fmt_seq P.braces items
  | List (items, tl) -> fmt_list items tl
  | Labeled (l, optional, value) -> fmt_labeled l optional value
  | _ -> P.string "$FMT"

and fmt_root (syn : Syntax.t) =
  match syn with
  | Block items ->
    if items = [] then P.empty
    else
      P.group
        (P.flow_map (P.semi ^^ P.twice P.hardline) (fmt ~enclose:false) items
        ^^ P.semi
        ^^ P.break 1
        )
  | _ -> fmt ~enclose:false syn

and fmt_binding lval rval =
  let lval_doc =
    match lval with
    | Apply _ | Form [ Syntax.Id (Lident ":"); Apply _; _ ] ->
      P.nest 2 (fmt ~enclose:false ~inline:true lval)
    | _ -> fmt ~enclose:false lval
  in
  match (lval, rval) with
  | _, _ when check_is_blockish rval ->
    P.group (lval_doc ^^ P.space ^^ P.string "=" ^^ P.space ^^ fmt rval)
  | Syntax.Id (Longident.Lident lid), Syntax.Id (Longident.Lident rid)
    when String.equal lid rid -> P.string "~" ^^ P.string lid
  (* [a = (b : t)] *)
  | _, Form (Syntax.Id (Lident ":") :: _) ->
    P.group
      (lval_doc
      ^^ P.blank 1
      ^^ P.string "="
      ^^ P.nest 2 (P.break 1 ^^ (fmt ~enclose:true) rval)
      )
  | _ ->
    P.group
      (lval_doc
      ^^ P.blank 1
      ^^ P.string "="
      ^^ P.nest 2 (P.break 1 ^^ (fmt ~enclose:false) rval)
      )

and fmt_arrow ~enclose a b =
  match (a, b) with
  | Form (Syntax.Id (Lident "fn") :: args), body ->
    fmt_arrow_fn ~enclose args body
  | _, Block _ ->
    (* No indent *)
    (* P.group (fmt ~enclose:false a ^^ P.string " -> " ^^ (fmt ~enclose:false) b) *)
    P.group
      (fmt ~enclose:false a
      ^^ P.string " -> "
      ^^ P.nest 2 ((fmt ~enclose:false) b)
      )
  | _ ->
    P.group
      (fmt ~enclose:false a
      ^^ P.string " ->"
      ^^ P.nest 2 (P.break 1 ^^ (fmt ~enclose:false) b)
      )

and fmt_arrow_fn ~enclose args body =
  if check_is_blockish body then
    P.group
      (P.parens_on enclose
         (P.string "fn "
         ^^ P.flow_map (P.break 1) fmt args
         ^^ P.string " -> "
         ^^ fmt body
         )
      )
  else
    let body_doc, ret_typ_doc =
      match body with
      | Form [ Syntax.Id (Lident ""); body; t ] ->
        (fmt ~enclose:false body, !^" : " ^^ fmt t)
      | _ -> (fmt ~enclose:false body, P.empty)
    in
    match args with
    | [] -> P.group (P.parens_on enclose (P.string "fn " ^^ body_doc))
    | _ ->
      P.group
        (P.parens_on enclose
           (P.string "fn "
           ^^ P.flow_map (P.break 1) fmt args
           ^^ ret_typ_doc
           ^^ P.string " ->"
           ^^ P.nest 2 (P.break 1 ^^ body_doc)
           )
        )

and fmt_constraint ~enclose v t =
  if check_is_blockish t then
    P.group
      (P.parens_on enclose
         (fmt ~enclose:false v ^^ P.string " : " ^^ (fmt ~enclose:false) t)
      )
  else
    P.group
      (P.parens_on enclose
         (fmt ~enclose:false v
         ^^ P.string " : "
         ^^ P.nest 2 ((fmt ~enclose:false) t)
         )
      )

and fmt_block items =
  if items = [] then P.braces P.empty
  else
    P.group
      (P.braces
         (P.nest 2
            (P.break 1
            ^^ P.flow_map (P.semi ^^ P.hardline) (fmt ~enclose:false) items
            ^^ P.semi
            )
         ^^ P.break 1
         )
      )

and fmt_seq brackets items =
  if items = [] then brackets P.empty
  else
    P.group
      (brackets
         (P.nest 2
            (P.flow_map (P.comma ^^ P.break 1) (fmt ~enclose:false) items)
         )
      )

and fmt_list items tl =
  if items = [] then
    match tl with
    | Some tl -> fmt ~enclose:false tl
    | None -> P.brackets P.empty
  else
    let tl_doc =
      match tl with
      | None -> P.empty
      | Some tl -> P.string ", .." ^^ fmt ~enclose:false tl
    in
    P.group
      (P.brackets
         (P.nest 2
            (P.break 0
            ^^ P.flow_map (P.comma ^^ P.break 1) (fmt ~enclose:false) items
            )
         ^^ tl_doc
         ^^ P.break 0
         )
      )

and fmt_juxt ~inline items =
  (* let juxt_sep = P.comma in *)
  let juxt_sep = P.empty in
  let sep_after = function
    | Syntax.Form (Id (Lident "fn") :: _) -> juxt_sep ^^ P.hardline
    | _ -> juxt_sep ^^ P.break 1
  in
  P.flow_map_sep ~inline ~sep_after fmt items

and fmt_apply ~enclose ~inline f args =
  match (f, args) with
  | _, [] -> fmt f
  (* [x |> f], [f <| x] *)
  | Syntax.Id (Longident.Lident (("|>" | "<|") as op)), [ arg_1; arg_2 ] ->
    let doc_1 = fmt ~enclose:false arg_1 in
    let doc_2 = fmt ~enclose:false arg_2 in
    P.parens_on enclose
      (P.group (doc_1 ^^ P.hardline ^^ P.string op ^^ P.space ^^ doc_2))
    (* Infix blockish: [a <*> {...}] *)
  | Syntax.Id (Longident.Lident op), [ arg_1; arg_2 ]
    when Fold_syntax.Operators.is_infix op && check_is_blockish arg_2 ->
    let doc_1 = fmt ~enclose:false arg_1 in
    let doc_2 = fmt ~enclose:false arg_2 in
    P.parens_on enclose
      (P.group (doc_1 ^^ P.space ^^ P.string op ^^ P.space ^^ doc_2))
  (* Infix op *)
  | Syntax.Id (Longident.Lident op), [ arg_1; arg_2 ]
    when Fold_syntax.Operators.is_infix op ->
    let doc_1 = fmt ~enclose:false arg_1 in
    let doc_2 = fmt ~enclose:false arg_2 in
    P.parens_on enclose (P.group (doc_1 ^/^ P.string op ^^ P.space ^^ doc_2))
  (* Prefix *)
  | Syntax.Id (Longident.Lident op), [ arg ]
    when Fold_syntax.Operators.is_prefix op ->
    P.string op ^^ fmt ~enclose:false arg
  (* Single block-ish arg. *)
  | _, [ arg ] when check_is_blockish arg ->
    P.parens_on enclose <| P.group (fmt f ^^ P.space ^^ fmt_juxt ~inline args)
  | _ ->
    P.parens_on enclose
    <| P.group (fmt f ^^ P.nest 2 (P.space ^^ fmt_juxt ~inline args))

and fmt_form_ items = P.group (P.flow_map (P.break 1) (fmt ~enclose:false) items)

and fmt_form ~enclose items =
  let out =
    match items with
    | [] -> P.empty
    (* let rec *)
    | Syntax.(Id (Lident "let") :: Id (Lident "rec") :: bindings) ->
      P.group
        (!^"let rec"
        ^^ P.concat_map
             (function
               | x ->
                 ( if Fold_syntax.Syntax_builder.is_binding x then P.space
                   else P.break 1
                 )
                 ^^ fmt x
               )
             bindings
        )
    (* let *)
    | Syntax.(Id (Lident "let") :: bindings) ->
      P.group
        (!^"let"
        ^^ P.concat_map
             (function
               | x ->
                 ( if Fold_syntax.Syntax_builder.is_binding x then P.space
                   else P.break 1
                 )
                 ^^ fmt x
               )
             bindings
        )
    (* val|module rec *)
    | Syntax.(
        Id (Lident (("val" | "module") as kwd))
        :: Id (Lident "rec")
        :: (form :: _ as bindings))
      when Fold_syntax.Syntax_builder.is_binding form ->
      (* :: (Syntax.Form (Id (Lident "=") :: _) :: _ as bindings)) -> *)
      (* :: (Syntax.Binding _ :: _ as bindings)) -> *)
      P.group
        (!^(kwd ^ " " ^ "rec")
        ^^ P.concat_map
             (function
               | x ->
                 ( if Fold_syntax.Syntax_builder.is_binding x then P.space
                   else P.twice P.hardline
                 )
                 ^^ fmt x
               )
             bindings
        )
    (* val|module *)
    | Syntax.(
        Id (Lident (("val" | "module") as kwd)) :: (form :: _ as bindings))
      when Fold_syntax.Syntax_builder.is_binding form ->
      (* :: (Syntax.Binding _ :: _ as bindings)) -> *)
      P.group
        (!^kwd
        ^^ P.concat_map
             (function
               | x ->
                 ( if Fold_syntax.Syntax_builder.is_binding x then P.space
                   else P.twice P.hardline
                 )
                 ^^ fmt x
               )
             bindings
        )
    | Syntax.[ Id (Lident "if"); cond; Id (Lident "then"); if_true ] ->
      P.group
        (!^"if"
        ^^ P.space
        ^^ fmt ~enclose:false cond
        ^^ P.space
        ^^ !^"then"
        ^^ begin
             if Syntax.is_block if_true then
               P.space ^^ fmt ~enclose:false if_true
             else P.nest 2 (P.break 1 ^^ fmt ~enclose:false if_true)
           end
        )
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
        ^^ fmt ~enclose:false cond
        ^^ P.space
        ^^ !^"then"
        ^^ begin
             if Syntax.is_block if_true then
               P.space ^^ fmt ~enclose:false if_true
             else P.nest 2 (P.break 1 ^^ fmt ~enclose:false if_true)
           end
        ^^ P.break 1
        ^^ !^"else"
        ^^ begin
             if Syntax.is_block if_false then
               P.space ^^ fmt ~enclose:false if_false
             else P.nest 2 (P.break 1 ^^ fmt ~enclose:false if_false)
           end
        )
    | _ -> P.group (P.flow_map P.space (fmt ~enclose:false) items)
  in
  P.parens_on enclose out

and fmt_record r0 fields =
  let fields_doc = P.flow_map (P.comma ^^ P.hardline) fmt fields in
  let r0_doc =
    match r0 with
    | Some r0 -> P.comma ^/^ P.string ".." ^^ fmt r0
    | None -> P.empty
  in
  P.group
    (P.lbrace
    ^^ P.nest 2 (P.break 1 ^^ fields_doc ^^ r0_doc)
    ^^ P.break 1
    ^^ P.rbrace
    )

and fmt_labeled l optional value =
  let map_to_optional_id (syn : Syntax.t) : Syntax.t =
    match syn with
    (* id = rval *)
    | Form [ Id (Lident "="); Id (Lident id); rval ] ->
      Form [ Id (Lident "="); Id (Lident (id ^ "?")); rval ]
    (* id : rval *)
    | Form [ Id (Lident ":"); Id (Lident id); rval ] ->
      Form [ Id (Lident ":"); Id (Lident (id ^ "?")); rval ]
    (* (id : typ) = rval *)
    | Form
        [ Id (Lident "="); Form [ Id (Lident ":"); Id (Lident id); typ ]; rval ]
      ->
      Form
        [ Id (Lident "=")
        ; Form [ Id (Lident ":"); Id (Lident (id ^ "?")); typ ]
        ; rval
        ]
    | _ -> syn
  in
  let enclose = Fold_syntax.Syntax_builder.is_binding value in
  match value with
  | Id (Lident id) when String.equal id l ->
    if optional then P.string "~" ^^ P.string l ^^ P.string "?"
    else P.string "~" ^^ P.string l
  | Form
      [ Id (Lident "=")
      ; (Id (Lident id) | Form [ Id (Lident ":"); Id (Lident id); _ ])
      ; _
      ]
  | Form [ Id (Lident ":"); Id (Lident id); _ ] ->
    if String.equal id l then
      let constraint' = if optional then map_to_optional_id value else value in
      P.string "~" ^^ P.parens_on enclose (fmt ~enclose:true constraint')
    else
      P.string "~"
      ^^ P.string l
      ^^ (if optional then P.string "?:" else P.string ":")
      ^^ P.parens_on enclose (fmt value)
  | _ ->
    P.string "~"
    ^^ P.string l
    ^^ P.string (if optional then "?:" else ":")
    ^^ P.parens_on enclose (fmt value)

and fmt_field a b = fmt a ^^ P.string "." ^^ fmt b

and fmt_or_pipe_no_braces items =
  if items = [] then P.braces P.empty
  else
    P.group
      (P.break 1
      ^^ P.string "| "
      ^^ P.flow_map (P.hardline ^^ P.string "| ") (fmt ~enclose:false) items
      ^^ P.break 1
      )

and fmt_cases items =
  if items = [] then P.braces P.empty
  else
    P.group
      (P.braces
         (P.break 1
         ^^ P.string "| "
         ^^ P.flow_map (P.hardline ^^ P.string "| ") (fmt ~enclose:false) items
         ^^ P.break 1
         )
      )

and fmt_or_indent items =
  if items = [] then P.braces P.empty
  else
    P.group
      (P.braces
         (P.nest 2
            (P.break 1
            ^^ P.flow_map (P.comma ^^ P.hardline) (fmt ~enclose:false) items
            )
         ^^ P.break 1
         )
      )

let print syntax =
  let doc = fmt_root syntax in
  P.ToChannel.pretty 0.8 80 stdout doc;
  print_newline ()
