type loc =
  { start_line : int; start_column : int; end_line : int; end_column : int }

let noloc = { start_line = 0; start_column = 0; end_line = 0; end_column = 0 }

type ident = Upper of string | Lower of string
type const = Int of int | Char of char | String of string | Float of float

type syntax =
  | Ident of ident
  | Const of const
  | Sym of string
  | Scope of string * syntax * string
  | Seq of string option * syntax list
  | Shape of string * syntax list

let lower x = Ident (Lower x)
let upper x = Ident (Upper x)
let sym x = Sym x
let int x = Const (Int x)
let char x = Const (Char x)
let string x = Const (String x)
let float x = Const (Float x)
let parens x = Scope ("(", x, ")")
let brackets x = Scope ("[", x, "]")
let braces x = Scope ("{", x, "}")
let seq ?sep items = Seq (sep, items)
let seq_comma items = Seq (Some ",", items)
let seq_semi items = Seq (Some ";", items)
let shape kwd items = Shape (kwd, items)

let is_scope = function
  | Scope _ -> true
  | _ -> false

let is_seq = function
  | Seq _ -> true
  | _ -> false

let is_seq_juxt = function
  | Seq (None, _) -> true
  | _ -> false

let is_shape = function
  | Shape _ -> true
  | _ -> false

let pp_ident f ident =
  let (Lower id | Upper id) = ident in
  Fmt.string f id

let pp_const f const =
  match const with
  | Int x -> Fmt.int f x
  | Float x -> Fmt.float f x
  | Char x -> Fmt.char f x
  | String x -> Fmt.Dump.string f x

let pp_sep sep f () = Fmt.string f sep

let rec dump f t =
  match t with
  | Ident ident -> Fmt.pf f "(Ident %a)" pp_ident ident
  | Sym x -> Fmt.pf f "(Sym %S)" x
  | Const const -> Fmt.pf f "(Const %a)" pp_const const
  | Seq (sep_opt, items) ->
    Fmt.pf f "(@[<hv1>Seq (%a,@ %a)@])"
      Fmt.Dump.(option string)
      sep_opt (Fmt.Dump.list dump) items
  | Scope (left, x, right) ->
    Fmt.pf f "(@[<hv1>Scope (%S,@,%a,@ %S)@])" left dump x right
  | Shape (kwd, items) ->
    Fmt.pf f "(@[<hv1>Shape (%S,@ %a)@])" kwd (Fmt.Dump.list dump) items

let rec pp_sexp f t =
  match t with
  | Ident ident -> Fmt.pf f "%a" pp_ident ident
  | Sym x -> Fmt.pf f "%s" x
  | Const const -> Fmt.pf f "%a" pp_const const
  | Seq (None, items) ->
    Fmt.pf f "(@[<hv1>%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) items
  | Seq (Some ";", items) ->
    Fmt.pf f "(@[<v1>;@ %a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) items
  | Seq (Some sep, items) ->
    Fmt.pf f "(@[<hv1>%s@ %a@])" sep (Fmt.list ~sep:Fmt.sp pp_sexp) items
  | Scope (left, Seq (None, []), right) -> Fmt.pf f "%s%s" left right
  | Scope (left, Seq (Some sep, items), right) ->
    Fmt.pf f "%s@[<hv1>%s@ %a@]%s" left sep
      (Fmt.list ~sep:Fmt.sp pp_sexp)
      items right
  | Scope (left, x, right) -> Fmt.pf f "%s@[<hv1>%a@]%s" left pp_sexp x right
  | Shape (kwd, items) ->
    Fmt.pf f "!(@[<hv1>%s@ %a@])" kwd (Fmt.list ~sep:Fmt.sp pp_sexp) items

let rec pp f t =
  match t with
  | Ident ident -> pp_ident f ident
  | Sym x -> Fmt.string f x
  | Const const -> pp_const f const
  | Seq (None, items) ->
    Fmt.pf f "<@[<hov1>%a@]>" (Fmt.list ~sep:Fmt.sp pp) items
  | Seq (Some sep, items) ->
    Fmt.pf f "<@[<hov1>%a@]>" (Fmt.list ~sep:(pp_sep sep) pp) items
  | Scope (left, x, right) -> Fmt.pf f "%s@[<hov1>%a@]%s" left pp x right
  | Shape (kwd, items) ->
    Fmt.pf f "<@@@[<hov1>%s %a@]>" kwd (Fmt.list ~sep:Fmt.sp pp) items
