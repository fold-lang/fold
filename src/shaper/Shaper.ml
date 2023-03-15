(*
 - Atom 'a
 - Call t * t list
 - Form string * t list
 - List t list
 *)

module V02 = struct
  type loc
  type ident = string
  type const = Int of int | Char of char | String of string | Float of float

  type syntax =
    | Ident of ident
    | Const of const
    | Group of (string * string option * string * syntax list)

  let ident x = Ident x
  let int x = Const (Int x)
  let char x = Const (Char x)
  let string x = Const (String x)
  let float x = Const (Float x)
  let parens ?sep items = Group ("(", sep, ")", items)
  let brackets ?sep items = Group ("[", sep, "]", items)
  let braces ?sep items = Group ("{", sep, "}", items)
  let group ?sep items = Group ("", sep, "", items)
  let pp_sep sep ppf () = Fmt.string ppf sep

  let rec pp : Format.formatter -> syntax -> unit =
   fun f t ->
    match t with
    | Ident id -> Fmt.string f id
    | Const (Int x) -> Fmt.int f x
    | Const (Float x) -> Fmt.float f x
    | Const (Char x) -> Fmt.char f x
    | Const (String x) -> Fmt.string f x
    | Group (gs, Some sep, ge, items) ->
      Fmt.pf f "%s@[<hov1>%a@]%s" gs (Fmt.list ~sep:(pp_sep sep) pp) items ge
    | Group (gs, None, ge, items) ->
      Fmt.pf f "%s@[<hov1>%a@]%s" gs (Fmt.list ~sep:Fmt.sp pp) items ge
end

module V03 = struct
  type loc
  type ident = Upper of string | Lower of string
  type const = Int of int | Char of char | String of string | Float of float

  type syntax =
    | Ident of ident
    | Const of const
    | Sym of string
    | Scope of string * syntax * string
    | Seq of string option * syntax list

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

  let pp_ident f ident =
    let (Lower id | Upper id) = ident in
    Fmt.string f id

  let pp_const f const =
    match const with
    | Int x -> Fmt.int f x
    | Float x -> Fmt.float f x
    | Char x -> Fmt.char f x
    | String x -> Fmt.string f x

  let pp_sep sep f () = Fmt.string f sep

  let rec pp f t =
    match t with
    | Ident ident -> pp_ident f ident
    | Sym x -> Fmt.string f x
    | Const const -> pp_const f const
    | Seq (None, items) ->
      Fmt.pf f "@[<hov1>%a@]" (Fmt.list ~sep:Fmt.sp pp) items
    | Seq (Some sep, items) ->
      Fmt.pf f "@[<hov1>%a@]" (Fmt.list ~sep:(pp_sep sep) pp) items
    | Scope (left, x, right) -> Fmt.pf f "%s@[<hov1>%a@]%s" left pp x right

  let rec pp_verbose f t =
    match t with
    | Ident ident -> pp_ident f ident
    | Sym x -> Fmt.string f x
    | Const const -> pp_const f const
    | Seq (None, items) ->
      Fmt.pf f "<@[<hov1>%a@]>" (Fmt.list ~sep:Fmt.sp pp_verbose) items
    | Seq (Some sep, items) ->
      Fmt.pf f "<@[<hov1>%a@]>" (Fmt.list ~sep:(pp_sep sep) pp_verbose) items
    | Scope (left, x, right) ->
      Fmt.pf f "%s@[<hov1>%a@]%s" left pp_verbose x right
end

type 'a syntax =
  | Atom of 'a
  | List of 'a syntax
  | Call of 'a syntax * 'a syntax list
  | Form of string * 'a syntax list

type 'a t =
  | Atom of 'a
  | Apply of 'a t * 'a t list
  | Block of 'a t list
  | List of 'a t list * 'a t option
  | Record of 'a t list * 'a t option
  | Array of 'a t list
  | Tuple of 'a t list
  | Seq of 'a t * 'a t
  | Arrow of 'a t * 'a t
  | Constraint of 'a t * 'a t
  | Or of 'a t * 'a t
  | Binding of 'a t * 'a t
  | Field of 'a t * 'a t
  | Labeled of string * bool * 'a t
  | Form of 'a form

and 'a form = string * 'a t list

type 'a root = 'a form list
type atom = Num of float | Sym of string

let rec pp fmt t =
  match t with
  | Atom (Sym x) -> Fmt.string fmt x
  | Atom (Num f) -> Fmt.float fmt f
  | Apply (f, args) ->
    Fmt.pf fmt "@[<hov1>(%a %a)@]" pp f (Fmt.list ~sep:Fmt.sp pp) args
  | Block [ Block items ] | Block items ->
    Fmt.pf fmt "{@,@[<hv>%a@]@,}" (Fmt.list ~sep:Fmt.semi pp) items
  | List (items, None) ->
    Fmt.pf fmt "@[[@[<hov2>%a@]]@]" (Fmt.list ~sep:Fmt.comma pp) items
  | List (items, Some tl) ->
    Fmt.pf fmt "@[[@[<hov2>%a & %a@]]@]"
      (Fmt.list ~sep:Fmt.comma pp)
      items pp tl
  | Array items ->
    Fmt.pf fmt "@[{@[<hv2>%a@]}@]" (Fmt.list ~sep:Fmt.comma pp) items
  | Tuple items ->
    Fmt.pf fmt "@[(@[<hv2>%a@])@]" (Fmt.list ~sep:Fmt.comma pp) items
  | Record (fields, r0) -> (
    match r0 with
    | None ->
      Fmt.pf fmt "@[{@[<hov2>%a@]}@]" (Fmt.list ~sep:Fmt.comma pp) fields
    | Some r ->
      Fmt.pf fmt "@[{@[<hov2>..%a, %a@]}@]" pp r
        (Fmt.list ~sep:Fmt.comma pp)
        fields
  )
  | Arrow (t1, t2) -> Fmt.pf fmt "@[(%a@ ->@ %a)@]" pp t1 pp t2
  | Constraint (t1, t2) -> Fmt.pf fmt "(%a : %a)" pp t1 pp t2
  | Or (t1, t2) -> Fmt.pf fmt "@[%a | %a@]" pp t1 pp t2
  | Seq (t1, t2) -> Fmt.pf fmt "@[<v1>(%a;@,%a)@]" pp t1 pp t2
  | Binding (t1, t2) -> Fmt.pf fmt "(%a = %a)" pp t1 pp t2
  | Field (t, id) -> Fmt.pf fmt "%a.%a" pp t pp id
  | Labeled (l, false, value) -> Fmt.pf fmt "~%s:(%a)" l pp value
  | Labeled (l, true, value) -> Fmt.pf fmt "~%s?:(%a)" l pp value
  | Form (kwd, items) ->
    Fmt.pf fmt "@[(%s! %a)@]" kwd (Fmt.list ~sep:Fmt.sp pp) items
