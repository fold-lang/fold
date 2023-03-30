type id = Longident.t =
  | Lident of string
  | Ldot of Longident.t * string
  | Lapply of Longident.t * Longident.t

type const = Parsetree.constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * Warnings.loc * string option
  | Pconst_float of string * char option

(*

Id id
Const const
Apply syntax (list syntax)       f x
Block syntax                     {_; ...; _}
List (list syntax)               [_, ..., _] []
Array (list syntax)              {_, ..., _} {}
Tuple (list syntax)              (_, ..., _) ()
Record (list syntax)             {l1=_, ..., ln=_}
Arrow syntax syntax              _ -> _
Or (list syntax)                 _ | ... | _
Binding syntax syntax            _ = _
Constraint syntax syntax         _ : _
Form (list string) (list syntax) if _ then _
Label string bool syntax         ~a?:b

fn a b -> a + b

val combine =
  fn a b ->
   | 0 b
   | a 0 -> 0
   | 0 0 -> 1
   | a b -> a + b

val combine =
  fn {
  | 0 b
  | a 0 -> 0
  | 0 0 -> 1
  | a b -> a + b
  }


val filter =
  fn {
  | 0 -> 1
  | a -> 1 + a
  }


(val (combine =
  (fn { })))


*)

type t =
  | Id of id  (** [x] [M.x] *)
  | Const of const  (** [1] ['x'] [3.14] ["abc"] *)
  | Apply of t * t list
      (** Application syntax for functions, constructors and types:

          - [f a b]
          - [C a b]
          - [t a b] *)
  | Block of t list  (** [{ x1; x2; xn; }] *)
  | List of t list * t option  (** [\[x1, x2, xn & xs\]] *)
  | Array of t list  (** [{x1, x2, xn}] *)
  | Tuple of t list  (** [(x1, x2, xn)] *)
  | Record of t list * t option
      (** - [{ l1=x1, l2=x2, ln=xn }]
          - [{ l1=x1, l2=x2, ln=xn, ..x0 }]
          - [{ l1=x1, l2=x2, ln=xn, _ }] *)
  | Labeled of string * bool * t
      (** [Labeled (l, optional, value)]:

          - [~l] when [optional] is [false] and [value] is [Id l].
          - [~l?] when [optional] is [true] and [value] is [Id l].
          - [~l:value] when [optional] is [false].
          - [~l?:value] when [optional] is [true].
          - [~(l : t)] when [optional] is [false] and [value] is
            [Constraint (Id l, _)].
          - [~(l? : t)] when [optional] is [true] and [value] is
            [Constraint (Id l, _)].
          - [~(l = t)] when [optional] is [false] and [value] is
            [Binding (Id l, _)].
          - [~(l? = t)] when [optional] is [false] and [value] is
            [Binding (Id l, _)]. *)
  | Form of t list  (** kwd1 x1 kwd2 x2 *)

let id ?(path = []) id = Id (Option.get (Longident.unflatten (path @ [ id ])))

let is_block = function
  | Block _ -> true
  | _ -> false

let pp_const f (const : const) =
  let open Pp_helpers in
  match const with
  | Pconst_char i -> Fmt.pf f "%C" i
  | Pconst_string (i, _, None) -> Fmt.pf f "%S" i
  | Pconst_string (i, _, Some delim) -> Fmt.pf f "{%s|%s|%s}" delim i delim
  | Pconst_integer (i, None) -> paren (first_is '-' i) (fun f -> pf f "%s") f i
  | Pconst_integer (i, Some m) ->
    paren (first_is '-' i) (fun f (i, m) -> pf f "%s%c" i m) f (i, m)
  | Pconst_float (i, None) -> paren (first_is '-' i) (fun f -> pf f "%s") f i
  | Pconst_float (i, Some m) ->
    paren (first_is '-' i) (fun f (i, m) -> pf f "%s%c" i m) f (i, m)

let pp_lident out s =
  match s.[0] with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '(' -> Fmt.string out s
  | _ -> Fmt.parens Fmt.string out s

let rec pp_id out (id : id) =
  match id with
  | Lident "" -> ()
  | Lident s -> pp_lident out s
  | Ldot (y, s) -> Fmt.pf out "%a.%a" pp_id y pp_lident s
  | Lapply (y, s) -> Fmt.pf out "%a(%a)" pp_id y pp_id s

let rec pp_syn fmt t =
  match t with
  | Id id -> pp_id fmt id
  | Const const -> pp_const fmt const
  | Apply (f, args) ->
    Fmt.pf fmt "@[<hov1>(%a %a)@]" pp_syn f (Fmt.list ~sep:Fmt.sp pp_syn) args
  | Block [ Block items ] | Block items ->
    Fmt.pf fmt "{@,@[<hv>%a@]@,}" (Fmt.list ~sep:Fmt.semi pp_syn) items
  | List (items, None) ->
    Fmt.pf fmt "@[[@[<hov2>%a@]]@]" (Fmt.list ~sep:Fmt.comma pp_syn) items
  | List (items, Some tl) ->
    Fmt.pf fmt "@[[@[<hov2>%a & %a@]]@]"
      (Fmt.list ~sep:Fmt.comma pp_syn)
      items pp_syn tl
  | Array items ->
    Fmt.pf fmt "@[{@[<hv2>%a@]}@]" (Fmt.list ~sep:Fmt.comma pp_syn) items
  | Tuple items ->
    Fmt.pf fmt "@[(@[<hv2>%a@])@]" (Fmt.list ~sep:Fmt.comma pp_syn) items
  | Record (fields, r0) -> (
    match r0 with
    | None ->
      Fmt.pf fmt "@[{@[<hov2>%a@]}@]" (Fmt.list ~sep:Fmt.comma pp_syn) fields
    | Some r ->
      Fmt.pf fmt "@[{@[<hov2>..%a, %a@]}@]" pp_syn r
        (Fmt.list ~sep:Fmt.comma pp_syn)
        fields
  )
  | Labeled (l, false, value) -> Fmt.pf fmt "~%s:(%a)" l pp_syn value
  | Labeled (l, true, value) -> Fmt.pf fmt "~%s?:(%a)" l pp_syn value
  | Form items -> Fmt.pf fmt "@[!(%a)@]" (Fmt.list ~sep:Fmt.sp pp_syn) items

let pp fmt t =
  match t with
  | Block items -> Fmt.pf fmt "@[<v>%a@]" (Fmt.list ~sep:Fmt.semi pp_syn) items
  | _ -> pp_syn fmt t

module V3 = struct
  type t =
    | Ident of Shaper.ident
    | Const of Shaper.const
    | Op of string
    | Block of t list
    | Record of t list * t option
    | List of t list * t option
    | Array of t list
    | Tuple of t list
    | Apply of t * t list
    | Binding of t * t
    | Let of (t * t) list * t
    | Val of (t * t) list
    | Fn of (t list * t) list
    | Match of t * (t * t) list
    | If_then_else of t * t * t option
    | Quote of t
    | Splice of t

  let ident x = Ident x
  let const x = Const x
  let op x = Op x
  let block xs = Block xs
  let record ?spread:r fields = Record (fields, r)
  let list ?spread:l xs = List (xs, l)
  let array xs = Array xs
  let tuple xs = Tuple xs
  let unit = Tuple []
  let apply f xs = Apply (f, xs)
  let binding lhs rhs = Binding (lhs, rhs)
  let let' bindings body = Let (bindings, body)
  let val' bindings = Val bindings
  let fn cases = Fn cases
  let match_ exp cases = Match (exp, cases)
  let if_then_else a b c = If_then_else (a, b, c)
  let quote x = Quote x
  let splice x = Splice x
end
