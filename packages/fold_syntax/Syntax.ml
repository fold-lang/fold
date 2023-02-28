type id = Longident.t
type const = Parsetree.constant

(*
  {Prefix interleave}
  if _ then _ else _
  match _ with _
  while _ do _
  for _ to _ do _
  for _ downto _ do _
  fn _ -> _
  object self { _ }

  {Multi prefix}
  module type _
  let rec _
  module rec _
  class type _
  module type of _
  type nonrec _
  inherit _ as _

  {Single prefix}
  object _
  let _
  open _
  include _
  type _
  exception _
  external _
  val _
  initializer _
  inherit _

  {Enclose sep}
  { _; ...; _ }
  { _, ..., _ }
  ( _, ..., _ )
  [ _, ..., _ ]

  {Sep}
  _; ...; _


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


Labeled punned:
f ~a

Labeled unpunned:
f ~a:b

Optional punned:
f ~a?

Optional unpunned:
f ~a?:b

---

Labeled:
fn ~a -> ...

Labeled with alias:
fn ~a:b -> ...

Labeled with type constraint:
fn ~(a : t) -> ...

Labeled with type constraint and alias:
fn ~a:(b : int) -> ...

Optional:
fn ~a? -> ...

Optional with alias:
fn ~a?:b -> ...

Optional with default:
fn ~(a = 1) -> ...

Optional with default and alias:
fn ~a:(b = 1) -> ...

Optional with type constraint and alias:
fn ~a:(b : int) -> ...

Optional with type constraint, alias and default:
fn ~a:(x : int = 42) -> ...
*)

type t =
  | Id of id  (** [x] [M.x] *)
  | Const of const  (** [1] ['x'] [3.14] ["abc"] *)
  | Apply of t * t list  (** [f a b] *)
  | Block of t list  (** [{ x1; x2; xn; }] *)
  | List of t list  (** [\[x1, x2, xn \]] *)
  | Array of t list  (** [{x1, x2, xn}] *)
  | Tuple of t list  (** [(x1, x2, xn)] *)
  | Record of t option * t list
      (** [{ l1=x1, l2=x2, ln=xn }] [{ ..x0, l1=x1, l2=x2, ln=xn }] *)
  | Arrow of t * t  (** x1 -> x2 *)
  | Fn of t list * t  (** fn x1 x2 *)
  | Or of t list  (** x1 | x2 | x3 *)
  | As of t * t  (** x1 as x2 *)
  | Binding of t * t  (** x1 = x2 *)
  | Field of t * id  (** x.l *)
  | Labeled of string * bool * t
  | Form of t list  (** kwd1 x1 kwd2 x2 *)

let id ?(path = []) id = Id (Option.get (Longident.unflatten (path @ [ id ])))

let is_binding = function
  | Binding _ -> true
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

let pp_id = Pprintast.longident

let protect_longident ppf print_longident longprefix txt =
  let format : (_, _, _) format = "%a.%s" in
  Format.fprintf ppf format print_longident longprefix txt

let rec pp_id out (id : id) =
  match id with
  | Lident s -> Fmt.string out s
  | Ldot (y, s) -> Fmt.pf out "%a.%s" pp_id y s
  | Lapply (y, s) -> Fmt.pf out "%a(%a)" pp_id y pp_id s

let rec pp_syn fmt t =
  match t with
  | Id id -> pp_id fmt id
  | Const const -> pp_const fmt const
  | Apply (f, args) ->
    Fmt.pf fmt "@[<hov1>(%a %a)@]" pp_syn f (Fmt.list ~sep:Fmt.sp pp_syn) args
  | Block [ Block items ] | Block items ->
    Fmt.pf fmt "{@,@[<hv>%a@]@,}" (Fmt.list ~sep:Fmt.semi pp_syn) items
  | List items ->
    Fmt.pf fmt "@[[@[<hov2>%a@]]@]" (Fmt.list ~sep:Fmt.comma pp_syn) items
  | Array items ->
    Fmt.pf fmt "@[{@[<hv2>%a@]}@]" (Fmt.list ~sep:Fmt.comma pp_syn) items
  | Tuple items ->
    Fmt.pf fmt "@[(@[<hv2>%a@])@]" (Fmt.list ~sep:Fmt.comma pp_syn) items
  | Record (r0, fields) -> (
    match r0 with
    | None ->
      Fmt.pf fmt "@[{@[<hov2>%a@]}@]" (Fmt.list ~sep:Fmt.comma pp_syn) fields
    | Some r ->
      Fmt.pf fmt "@[{@[<hov2>..%a, %a@]}@]" pp_syn r
        (Fmt.list ~sep:Fmt.comma pp_syn)
        fields)
  | Arrow (t1, t2) -> Fmt.pf fmt "%a -> %a" pp_syn t1 pp_syn t2
  | Fn (args, body) ->
    Fmt.pf fmt "@[<2>(fn %a -> %a)@]"
      (Fmt.list ~sep:Fmt.sp pp_syn)
      args pp_syn body
  | Or cases -> Fmt.pf fmt "@[%a@]" (Fmt.list ~sep:(Fmt.any "|") pp_syn) cases
  | As (t1, t2) -> Fmt.pf fmt "@[(%a as %a)@]" pp_syn t1 pp_syn t2
  | Binding (t1, t2) -> Fmt.pf fmt "%a = %a" pp_syn t1 pp_syn t2
  | Field (t, id) -> Fmt.pf fmt "%a.%a" pp_syn t pp_id id
  | Labeled (l, false, value) -> Fmt.pf fmt "~%s:(%a)" l pp_syn value
  | Labeled (l, true, value) -> Fmt.pf fmt "~%s?:(%a)" l pp_syn value
  | Form items -> Fmt.pf fmt "@[!(%a)@]" (Fmt.list ~sep:Fmt.sp pp_syn) items

let pp fmt t =
  match t with
  | Block items -> Fmt.pf fmt "@[<v>%a@]" (Fmt.list ~sep:Fmt.semi pp_syn) items
  | _ -> pp_syn fmt t
