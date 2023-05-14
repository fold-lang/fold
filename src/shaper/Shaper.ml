let noloc = Astlib.Location.none

type ident = Upper of string | Lower of string
type const = Int of int | Char of char | String of string | Float of float

type syntax =
  | Ident of ident
  | Const of const
  | Sym of string
  | Scope of string * syntax * string
  | Seq of syntax list
  | Shape of Astlib.Location.t * string * syntax list

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
let seq items = Seq items
let comma ?(loc = noloc) items = Shape (loc, ",", items)
let semi ?(loc = noloc) items = Shape (loc, ";", items)
let shape ?(loc = noloc) kwd items = Shape (loc, kwd, items)

let is_scope = function
  | Scope _ -> true
  | _ -> false

let is_seq = function
  | Seq _ -> true
  | _ -> false

let is_seq_juxt = function
  | Seq _ -> true
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
  | Seq items -> Fmt.pf f "(@[<hv1>Seq (%a)@])" (Fmt.Dump.list dump) items
  | Scope (left, x, right) ->
    Fmt.pf f "(@[<hv1>Scope (%S,@,%a,@ %S)@])" left dump x right
  | Shape (_loc, kwd, items) ->
    Fmt.pf f "(@[<hv1>Shape (%S,@ %a)@])" kwd (Fmt.Dump.list dump) items

let rec pp_sexp f t =
  match t with
  | Ident ident -> Fmt.pf f "%a" pp_ident ident
  | Sym x -> Fmt.pf f "%s" x
  | Const const -> Fmt.pf f "%a" pp_const const
  | Seq items -> Fmt.pf f "(@[<hv1>%a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) items
  | Shape (_, ";", items) ->
    Fmt.pf f "(@[<v1>;@ %a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) items
  | Shape (_, ",", items) ->
    Fmt.pf f "(@[<hv1>,@ %a@])" (Fmt.list ~sep:Fmt.sp pp_sexp) items
  | Scope (left, Seq [], right) -> Fmt.pf f "%s%s" left right
  | Scope (left, Shape (_, ((";" | ",") as sep), items), right) ->
    Fmt.pf f "%s@[<hv1>%s@ %a@]%s" left sep
      (Fmt.list ~sep:Fmt.sp pp_sexp)
      items right
  | Scope (left, x, right) -> Fmt.pf f "%s@[<hv1>%a@]%s" left pp_sexp x right
  | Shape (_loc, kwd, items) ->
    Fmt.pf f "!(@[<hv1>%s@ %a@])" kwd (Fmt.list ~sep:Fmt.sp pp_sexp) items

let rec pp f t =
  match t with
  | Ident ident -> pp_ident f ident
  | Sym x -> Fmt.string f x
  | Const const -> pp_const f const
  | Seq items -> Fmt.pf f "@[<hov1>%a@]" (Fmt.list ~sep:Fmt.sp pp) items
  | Scope (left, x, right) -> Fmt.pf f "%s @[<hov1>%a@] %s" left pp x right
  | Shape (_loc, kwd, items) ->
    Fmt.pf f "@[<hov1>%s! %a@]" kwd (Fmt.list ~sep:Fmt.sp pp) items
