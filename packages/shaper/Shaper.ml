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
