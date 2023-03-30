open Prelude
module Fl = Fold_ast
module C = Fold_ast.Cons

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

let ( ^^ ) = PPrint.( ^^ )
let ( !^ ) = PPrint.( !^ )
let ( ^/^ ) = PPrint.( ^/^ )

module Ast_eval = struct
  let eval ~ident ~const ~apply ~seq_semi ~seq_comma ~arrow ~constraint_
      ~binding ~cases ~field ~shape ~block ~parens ~braces ctx (fl : fl) =
    match fl with
    | Ident x -> ident ctx x
    | Const x -> const ctx x
    | Seq (None, f :: args) -> apply ctx f args
    | Seq (Some ";", items) -> seq_semi ctx items
    | Seq (Some ",", items) -> seq_comma ctx items
    | Shape ("->", [ a; b ]) -> arrow ctx a b
    | Shape (":", [ v; t ]) -> constraint_ ctx v t
    | Shape ("=", [ lhs; rhs ]) -> binding ctx lhs rhs
    | Shape ("|", items) -> cases ctx items
    | Shape (".", [ a; b ]) -> field ctx a b
    | Shape (kwd, items) -> shape ctx kwd items
    | Scope ("{", Seq (Some ";", items), "}") -> block ctx items
    | Scope ("(", Seq (Some ",", items), ")") -> parens ctx items
    | Scope ("{", Seq (Some ",", items), "}") -> braces ctx items
    | _ -> ident ctx (Lower "$FMT")
end

type ctx = { enclose : bool; inline : bool }

let rec fmt ?(ctx = { enclose = true; inline = false }) (fl : fl) =
  Ast_eval.eval ctx fl ~ident ~const ~apply ~seq_semi ~seq_comma ~arrow
    ~constraint_ ~binding ~cases ~field ~shape ~block ~parens ~braces

and ident _ctx id = P.string (Fmt.str "%a" C.pp_ident id)
and const _ctx const = P.string (Fmt.str "%a" C.pp_const const)

and binding ctx (lhs : fl) (rhs : fl) =
  let lval_doc =
    match lhs with
    | Seq (None, _) | Shape (":", [ Seq (None, _); _ ]) ->
      P.nest 2 (fmt ~ctx:{ enclose = false; inline = true } lhs)
    | _ -> fmt ~ctx:{ ctx with enclose = false } lhs
  in
  match (lhs, rhs) with
  | _, _ when C.is_scope rhs ->
    P.group (lval_doc ^^ P.space ^^ P.string "=" ^^ P.space ^^ fmt rhs)
  | Ident (Upper lid | Lower lid), Ident (Upper rid | Lower rid)
    when String.equal lid rid -> P.string "~" ^^ P.string lid
  (* [a = (b : t)] *)
  | _, Shape (":", _ :: _) ->
    P.group
      (lval_doc
      ^^ P.blank 1
      ^^ P.string "="
      ^^ P.nest 2 (P.break 1 ^^ (fmt ~ctx:{ ctx with enclose = true }) rhs)
      )
  | _ ->
    P.group
      (lval_doc
      ^^ P.blank 1
      ^^ P.string "="
      ^^ P.nest 2 (P.break 1 ^^ (fmt ~ctx:{ ctx with enclose = false }) rhs)
      )

and arrow ctx a b =
  match (a, b) with
  | Shape ("fn", args), body -> arrow_fn ctx args body
  | _, Scope ("{", _, "}") ->
    (* No indent *)
    P.group
      (fmt ~ctx:{ ctx with enclose = false } a
      ^^ P.string " -> "
      ^^ (fmt ~ctx:{ ctx with enclose = false }) b
      )
    (* P.group *)
    (*   (fmt ~ctx:{ ctx with enclose = false } a *)
    (*   ^^ P.string " -> " *)
    (*   ^^ P.nest 2 ((fmt ~ctx:{ ctx with enclose = false }) b) *)
    (*   ) *)
  | _ ->
    P.group
      (fmt ~ctx:{ ctx with enclose = false } a
      ^^ P.string " ->"
      ^^ P.nest 2 (P.break 1 ^^ (fmt ~ctx:{ ctx with enclose = false }) b)
      )

and arrow_fn ({ enclose; _ } as ctx) args body =
  if C.is_scope body then
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
      | Shape (":", [ body; t ]) ->
        (fmt ~ctx:{ ctx with enclose = false } body, !^" : " ^^ fmt t)
      | _ -> (fmt ~ctx:{ ctx with enclose = false } body, P.empty)
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

and constraint_ ctx v t =
  if C.is_scope t then
    P.group
      (P.parens_on ctx.enclose
         (fmt ~ctx:{ ctx with enclose = false } v
         ^^ P.string " : "
         ^^ (fmt ~ctx:{ ctx with enclose = false }) t
         )
      )
  else
    P.group
      (P.parens_on ctx.enclose
         (fmt ~ctx:{ ctx with enclose = false } v
         ^^ P.string " : "
         ^^ P.nest 2 ((fmt ~ctx:{ ctx with enclose = false }) t)
         )
      )

and block ctx items =
  if List_ext.is_empty items then P.braces P.empty
  else
    P.group
      (P.braces
         (P.nest 2
            (P.break 1
            ^^ P.flow_map (P.semi ^^ P.hardline)
                 (fmt ~ctx:{ ctx with enclose = false })
                 items
            ^^ P.semi
            )
         ^^ P.break 1
         )
      )

and seq_semi ctx items =
  P.group
    (P.flow_map
       (P.semi ^^ P.twice P.hardline)
       (fmt ~ctx:{ ctx with enclose = false })
       items
    ^^ P.semi
    ^^ P.break 1
    )

and seq_comma ctx items =
  P.group
    (P.flow_map
       (P.comma ^^ P.break 1)
       (fmt ~ctx:{ ctx with enclose = false })
       items
    )

and seq ctx brackets items =
  if List_ext.is_empty items then brackets P.empty
  else
    P.group
      (brackets
         (P.nest 2
            (P.flow_map
               (P.comma ^^ P.break 1)
               (fmt ~ctx:{ ctx with enclose = false })
               items
            )
         )
      )

and parens ctx xs = seq ctx P.parens xs
and braces ctx xs = seq ctx P.braces xs

and list ctx items tl =
  if List_ext.is_empty items then
    match tl with
    | Some tl -> fmt ~ctx:{ ctx with enclose = false } tl
    | None -> P.brackets P.empty
  else
    let tl_doc =
      match tl with
      | None -> P.empty
      | Some tl -> P.string ", .." ^^ fmt ~ctx:{ ctx with enclose = false } tl
    in
    P.group
      (P.brackets
         (P.nest 2
            (P.break 0
            ^^ P.flow_map
                 (P.comma ^^ P.break 1)
                 (fmt ~ctx:{ ctx with enclose = false })
                 items
            )
         ^^ tl_doc
         ^^ P.break 0
         )
      )

and juxt ~inline items =
  (* let juxt_sep = P.comma in *)
  let juxt_sep = P.empty in
  let sep_after = function
    | Shaper.Shape ("fn", _) -> juxt_sep ^^ P.hardline
    | _ -> juxt_sep ^^ P.break 1
  in
  P.flow_map_sep ~inline ~sep_after fmt items

and apply ({ enclose; inline } as ctx) f args =
  match (f, args) with
  | _, [] -> fmt f
  (* [x |> f], [f <| x] *)
  | Sym (("|>" | "<|") as op), [ arg_1; arg_2 ] ->
    let doc_1 = fmt ~ctx:{ ctx with enclose = false } arg_1 in
    let doc_2 = fmt ~ctx:{ ctx with enclose = false } arg_2 in
    P.parens_on enclose
      (P.group (doc_1 ^^ P.hardline ^^ P.string op ^^ P.space ^^ doc_2))
    (* Infix blockish: [a <*> {...}] *)
  | Sym op, [ arg_1; arg_2 ] when C.is_scope arg_2 ->
    let doc_1 = fmt ~ctx:{ ctx with enclose = false } arg_1 in
    let doc_2 = fmt ~ctx:{ ctx with enclose = false } arg_2 in
    P.parens_on enclose
      (P.group (doc_1 ^^ P.space ^^ P.string op ^^ P.space ^^ doc_2))
  (* Infix op *)
  | Sym op, [ arg_1; arg_2 ] ->
    let doc_1 = fmt ~ctx:{ ctx with enclose = false } arg_1 in
    let doc_2 = fmt ~ctx:{ ctx with enclose = false } arg_2 in
    P.parens_on enclose (P.group (doc_1 ^/^ P.string op ^^ P.space ^^ doc_2))
  (* Prefix *)
  | Sym op, [ arg ] -> P.string op ^^ fmt ~ctx:{ ctx with enclose = false } arg
  (* Single block-ish arg. *)
  | _, [ arg ] when C.is_scope arg ->
    P.parens_on enclose <| P.group (fmt f ^^ P.space ^^ juxt ~inline args)
  | _ ->
    P.parens_on enclose
    <| P.group (fmt f ^^ P.nest 2 (P.space ^^ juxt ~inline args))

and shape_ ctx items =
  P.group (P.flow_map (P.break 1) (fmt ~ctx:{ ctx with enclose = false }) items)

and shape ctx kwd items =
  match (kwd, items) with
  | _, [] -> P.string kwd
  (* let *)
  | "let", bindings ->
    P.group
      (!^"let"
      ^^ P.concat_map
           (function
             | x -> (if Fl.is_binding x then P.space else P.break 1) ^^ fmt x
             )
           bindings
      )
    |> P.parens_on ctx.enclose
  (* val|module *)
  | (("let" | "module") as kwd), (shape :: _ as bindings)
    when Fl.is_binding shape ->
    (* :: (Syntax.Binding _ :: _ as bindings)) -> *)
    P.group
      (!^kwd
      ^^ P.concat_map
           (function
             | x ->
               (if Fl.is_binding x then P.space else P.twice P.hardline)
               ^^ fmt x
             )
           bindings
      )
    |> P.parens_on ctx.enclose
  | "if", [ cond; Scope ("{", if_true, "}") ] ->
    P.group
      (!^"if"
      ^^ P.space
      ^^ fmt ~ctx:{ ctx with enclose = false } cond
      ^^ P.space
      ^^ !^"then"
      ^^ begin
           if C.is_scope if_true then
             P.space ^^ fmt ~ctx:{ ctx with enclose = false } if_true
           else
             P.nest 2
               (P.break 1 ^^ fmt ~ctx:{ ctx with enclose = false } if_true)
         end
      )
    |> P.parens_on ctx.enclose
  | "if", [ a; b; c ] ->
    P.group
      (!^"if"
      ^^ P.space
      ^^ fmt ~ctx:{ ctx with enclose = false } a
      ^^ P.space
      ^^ !^"then"
      ^^ begin
           if C.is_scope b then
             P.space ^^ fmt ~ctx:{ ctx with enclose = false } b
           else P.nest 2 (P.break 1 ^^ fmt ~ctx:{ ctx with enclose = false } b)
         end
      ^^ P.break 1
      ^^ !^"else"
      ^^ begin
           if C.is_scope b then
             P.space ^^ fmt ~ctx:{ ctx with enclose = false } b
           else P.nest 2 (P.break 1 ^^ fmt ~ctx:{ ctx with enclose = false } b)
         end
      )
    |> P.parens_on ctx.enclose
  | ".", items ->
    P.flow_map (P.string kwd) (fmt ~ctx:{ ctx with enclose = false }) items
  | "~", [ Ident (Lower l); value ] -> labeled ctx l false value
  | "~?", [ Ident (Lower l); value ] -> labeled ctx l true value
  | _ ->
    P.group
      (P.string kwd
      ^^ P.space
      ^^ P.flow_map P.space (fmt ~ctx:{ ctx with enclose = false }) items
      )
    |> P.parens_on ctx.enclose

and record r0 fields =
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

and labeled ctx l optional value =
  let map_to_optional_id (fl : fl) : fl =
    match fl with
    (* TODO Upper/lower *)
    (* id = rval *)
    | Shape ("=", [ Ident (Lower id); rval ]) ->
      C.shape "=" [ C.lower (id ^ "?"); rval ]
    (* id : rval *)
    | Shape (":", [ Ident (Lower id); rval ]) ->
      C.shape ":" [ C.lower (id ^ "?"); rval ]
    (* (id : typ) = rval *)
    | Shape ("=", [ Shape ("{", [ Ident (Lower id); typ ]); rval ]) ->
      C.shape "=" [ C.shape ":" [ C.lower (id ^ "?"); typ ]; rval ]
    | _ -> fl
  in
  let enclose = Fl.is_binding value in
  match value with
  | Ident (Lower id) when String.equal id l ->
    if optional then P.string "~" ^^ P.string l ^^ P.string "?"
    else P.string "~" ^^ P.string l
  | Shape ("=", [ (Ident (Lower id) | Shape (":", [ Ident (Lower id); _ ])); _ ])
  | Shape (":", [ Ident (Lower id); _ ]) ->
    if String.equal id l then
      let constraint' = if optional then map_to_optional_id value else value in
      P.string "~"
      ^^ P.parens_on enclose (fmt ~ctx:{ ctx with enclose = true } constraint')
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

and field _ctx a b = fmt a ^^ P.string "." ^^ fmt b

and or_pipe_no_braces ctx items =
  if List_ext.is_empty items then P.braces P.empty
  else
    P.group
      (P.break 1
      ^^ P.string "| "
      ^^ P.flow_map
           (P.hardline ^^ P.string "| ")
           (fmt ~ctx:{ ctx with enclose = false })
           items
      ^^ P.break 1
      )

and cases_ ctx items =
  if List_ext.is_empty items then P.braces P.empty
  else
    P.group
      (P.braces
         (P.break 1
         ^^ P.string "| "
         ^^ P.flow_map
              (P.hardline ^^ P.string "| ")
              (fmt ~ctx:{ ctx with enclose = false })
              items
         ^^ P.break 1
         )
      )

and cases ctx items =
  if List_ext.is_empty items then P.braces P.empty
  else
    P.group
      (P.braces
         (P.nest 2
            (P.break 1
            ^^ P.flow_map (P.comma ^^ P.hardline)
                 (fmt ~ctx:{ ctx with enclose = false })
                 items
            )
         ^^ P.break 1
         )
      )

let print chan syntax =
  let doc = fmt syntax in
  P.ToChannel.pretty 0.8 80 chan doc
