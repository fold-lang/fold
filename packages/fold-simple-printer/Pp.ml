let pf = Format.fprintf

type space_formatter = (unit, Format.formatter, unit) format

let paren :
      'a.
         ?first:space_formatter
      -> ?last:space_formatter
      -> bool
      -> (Format.formatter -> 'a -> unit)
      -> Format.formatter
      -> 'a
      -> unit =
 fun ?(first = ("" : _ format6)) ?(last = ("" : _ format6)) b fu f x ->
  if b then (
    pf f "(";
    pf f first;
    fu f x;
    pf f last;
    pf f ")")
  else fu f x

let prefix_symbols = [ '!'; '?'; '~' ]

let infix_symbols =
  [ '='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '#' ]

let special_infix_strings =
  [ "asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::" ]

let builtin_keywords = [ "let"; "module"; "type"; "val"; "..."; "~"; ":"; ":?" ]
let is_builtin_keyword s = List.mem s builtin_keywords

let letop s =
  String.length s > 3
  && s.[0] = 'l'
  && s.[1] = 'e'
  && s.[2] = 't'
  && List.mem s.[3] infix_symbols

let andop s =
  String.length s > 3
  && s.[0] = 'a'
  && s.[1] = 'n'
  && s.[2] = 'd'
  && List.mem s.[3] infix_symbols

(* determines if the string is an infix string.
   checks backwards, first allowing a renaming postfix ("_102") which
   may have resulted from Pexp -> Texp -> Pexp translation, then checking
   if all the characters in the beginning of the string are valid infix
   characters. *)
let fixity_of_string = function
  | "" -> `Normal
  | s when List.mem s special_infix_strings -> `Infix s
  | s when List.mem s.[0] infix_symbols -> `Infix s
  | s when List.mem s.[0] prefix_symbols -> `Prefix s
  | s when s.[0] = '.' -> `Mixfix s
  | s when letop s -> `Letop s
  | s when andop s -> `Andop s
  | _ -> `Normal

let first_is c str = str <> "" && str.[0] = c
let last_is c str = str <> "" && str.[String.length str - 1] = c
let pp_ident = Pprintast.longident

let pp_constant f (constant : Syntax.constant) =
  match constant with
  | Pconst_char i -> pf f "%C" i
  | Pconst_string (i, _, None) -> pf f "%S" i
  | Pconst_string (i, _, Some delim) -> pf f "{%s|%s|%s}" delim i delim
  | Pconst_integer (i, None) -> paren (first_is '-' i) (fun f -> pf f "%s") f i
  | Pconst_integer (i, Some m) ->
    paren (first_is '-' i) (fun f (i, m) -> pf f "%s%c" i m) f (i, m)
  | Pconst_float (i, None) -> paren (first_is '-' i) (fun f -> pf f "%s") f i
  | Pconst_float (i, Some m) ->
    paren (first_is '-' i) (fun f (i, m) -> pf f "%s%c" i m) f (i, m)

let rec pp enclose out (syn : Syntax.t) =
  match syn with
  | Void -> ()
  | Ident (Longident.Lident s) -> pf out "%s" s
  | Ident id -> pp_ident out id
  | Constant const -> pf out "%a" pp_constant const
  | Apply (f, args) -> pp_apply out enclose f args
  | Form (kwd, args) -> pp_form out enclose kwd args
  | Binding binding when enclose -> Fmt.pf out "@[(%a)@]" pp_binding binding
  | Binding binding -> Fmt.pf out "@[%a@]" pp_binding binding
  | Variant cases ->
    Fmt.pf out "{@ %a@ }" (Fmt.list ~sep:Fmt.cut (pp true)) cases
  | Seq items -> Fmt.pf out "@[%a@]" (Fmt.list ~sep:Fmt.comma (pp false)) items
  | Block [] -> Fmt.pf out "{}"
  | Block [ Seq items ] ->
    Fmt.pf out "@[<hov>@[<hov2>{@;%a@]@;}@]"
      (Fmt.list ~sep:Fmt.comma (pp false))
      items
  | Block [ item ] -> Fmt.pf out "@[<v>@[<v2>{@;%a@]@;}@]" (pp false) item
  | Block items ->
    Fmt.pf out "@[<v>@[<v2>{@;%a;@]@;}@]"
      (Fmt.list ~sep:Fmt.semi (pp false))
      items
  | Constraint (v, t) -> Fmt.pf out "@[(%a : %a)@]" (pp true) v (pp true) t
  | Field (x, f) -> Fmt.pf out "@[%a.%a@]" (pp true) x (pp true) f

and pp_binding out (left, right) =
  match (left, right) with
  | ( Syntax.Ident (Longident.Lident left_id)
    , Syntax.Ident (Longident.Lident right_id) )
    when left_id = right_id -> pf out "~%s" left_id
  | _ -> pf out "@[%a = %a@]" (pp false) left (pp false) right

and pp_apply out enclose f args =
  match f with
  | Ident (Longident.Lident id) -> (
    match (fixity_of_string id, args) with
    | `Infix _, [ arg_1; arg_2 ] when enclose ->
      pf out "@[<hv2>(%a@ %s@ %a)@]" (pp true) arg_1 id (pp true) arg_2
    | `Infix _, [ arg_1; arg_2 ] ->
      pf out "@[<hv2>%a@ %s@ %a@]" (pp true) arg_1 id (pp true) arg_2
    | `Prefix _, [ arg ] when enclose ->
      pf out "@[<hv2>(%s@ %a)@]" id (pp true) arg
    | `Prefix _, [ arg ] -> pf out "@[<hv2>%s@ %a@]" id (pp true) arg
    | _ when enclose ->
      pf out "@[<hv2>(%a@ %a)@]" (pp true) f
        (Fmt.list ~sep:Fmt.sp (pp true))
        args
    | _ -> pf out "@[%a@ %a@]" (pp true) f (Fmt.list ~sep:Fmt.sp (pp true)) args
    )
  | _ when enclose ->
    pf out "@[<hv2>(%a@ %a)@]" (pp true) f (Fmt.list ~sep:Fmt.sp (pp true)) args
  | _ -> pf out "@[%a@ %a@]" (pp true) f (Fmt.list ~sep:Fmt.sp (pp true)) args

and keyword_to_string kwd = if is_builtin_keyword kwd then kwd else kwd ^ "!"

and pp_form out enclose kwds args =
  match kwds with
  | [ kwd ] -> (
    let kwd' = keyword_to_string kwd in
    match args with
    | [] -> pf out "%s" kwd'
    | _ when enclose ->
      pf out "@[(%s %a)@]" kwd' (Fmt.list ~sep:Fmt.sp (pp false)) args
    | _ -> pf out "@[%s %a@]" kwd' (Fmt.list ~sep:Fmt.sp (pp false)) args)
  | [ "~"; l ] -> (
    match args with
    | [ Syntax.Ident (Longident.Lident v) ] when l = v -> pf out "~%s" l
    | [ v ] -> pf out "~%s:%a" l (pp false) v
    | _ -> invalid_arg "invalid labeled argument form")
  | [ "?"; l ] -> (
    match args with
    | [ Syntax.Ident (Longident.Lident v) ] when l = v -> pf out "~%s?" l
    | [ v ] -> pf out "~%s?:%a" l (pp false) v
    | _ -> invalid_arg "invalid labeled argument form")
  | _ ->
    pf out "(";
    List.iter2
      (fun kwd arg -> pf out "%s %a " (keyword_to_string kwd) (pp false) arg)
      kwds args;
    pf out ")"
