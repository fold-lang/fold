(* https://github.com/astahfrom/prettiest/blob/31ee502082dc864864e8125011fe3d85e7c399b9/lib/prettiest.ml *)
module List = struct
  include ListLabels

  let split_n t_orig n =
    if n <= 0 then ([], t_orig)
    else
      let rec loop n t accum =
        if n = 0 then (rev accum, t)
        else
          match t with
          | [] -> (t_orig, [] (* in this case, t_orig = rev accum *))
          | hd :: tl -> loop (n - 1) tl (hd :: accum)
      in
      loop n t_orig []

  let cartesian_product list1 list2 =
    if list2 = [] then []
    else
      let rec loop l1 l2 accum =
        match l1 with
        | [] -> accum
        | hd :: tl ->
          loop tl l2 (rev_append (map ~f:(fun x -> (hd, x)) l2) accum)
      in
      rev (loop list1 list2 [])

  let fold = fold_left

  let min_elt t ~compare =
    (fold_left t ~init:None ~f:(fun acc elt ->
         match acc with
         | None -> Some elt
         | Some min -> if compare min elt > 0 then Some elt else acc) [@nontail])
end

module Option = struct
  include Option

  let map t ~f = map f t
  let some_if cond x = if cond then Some x else None
end

module String = StringLabels

let const x _ = x
let replicate n x = List.init ~len:n ~f:(const x)
let replicate_char n c = String.init n ~f:(const c)

module type Layout = sig
  type t

  val text : string -> t
  val flush : t -> t
  val ( <> ) : t -> t -> t
  val render : t -> string
end

module type Poset = sig
  type t

  val ( << ) : t -> t -> bool
end

module Measure : sig
  type t

  val compare : t -> t -> int
  val valid : int -> t -> bool

  include Layout with type t := t
  include Poset with type t := t
end = struct
  type t = { height : int; max_width : int; last_width : int }

  let compare = Stdlib.compare
  let valid w x = x.max_width <= w

  let text s =
    { height = 0; last_width = String.length s; max_width = String.length s }

  let flush a =
    { height = a.height + 1; last_width = 0; max_width = a.max_width }

  let ( <> ) a b =
    { height = a.height + b.height
    ; last_width = a.last_width + b.last_width
    ; max_width = max a.max_width (a.last_width + b.max_width)
    }

  let render m =
    String.concat ~sep:"\n"
      (replicate m.height (replicate_char m.max_width 'x')
      @ [ replicate_char m.last_width 'x' ])

  let ( << ) m1 m2 =
    m1.height <= m2.height
    && m1.max_width <= m2.max_width
    && m1.last_width <= m2.last_width
end

module Text : Layout = struct
  type t = string list

  let render xs = String.concat ~sep:"\n" xs
  let text s = [ s ]

  let ( <> ) xs ys =
    match ys with
    | [] -> failwith "<>: empty ys"
    | y :: ys -> (
      let xs = xs in
      match List.split_n xs (List.length xs - 1) with
      | xs0, [ x ] ->
        let indent = replicate_char (String.length x) ' ' in
        xs0 @ [ x ^ y ] @ List.map ~f:(fun y -> indent ^ y) ys
      | _ -> failwith "<>: empty xs")

  let flush xs = xs @ [ "" ]
end

module MeasureText : sig
  type t

  val compare : t -> t -> int
  val valid : int -> t -> bool

  include Layout with type t := t
  include Poset with type t := t
end = struct
  type t = Measure.t * Text.t Lazy.t

  let compare (m, _) (m', _) = Measure.compare m m'
  let valid w (m, _) = Measure.valid w m
  let ( << ) (m1, _) (m2, _) = Measure.(m1 << m2)

  let ( <> ) (m1, a) (m2, b) =
    (Measure.(m1 <> m2), lazy Text.(Lazy.force a <> Lazy.force b))

  let flush (m, a) = (Measure.flush m, lazy (Text.flush (Lazy.force a)))
  let text s = (Measure.text s, lazy (Text.text s))
  let render (_, a) = Text.render (Lazy.force a)
end

module type Width = sig
  val width : int
end

module type S = sig
  type t

  include Width

  val text : string -> t
  val flush : t -> t
  val hcat : t list -> t
  val hsep : t list -> t
  val vcat : t list -> t
  val render : t -> string option
  val choice : t list -> t
  val fail : t
  val empty : t
  val nest : int -> t -> t
  val sep : t list -> t
  val intersperse : sep:t -> t list -> t list
  val intersperse_map : f:('a -> t) -> sep:t -> 'a list -> t list

  module Infix : sig
    val ( !^ ) : string -> t
    val ( <> ) : t -> t -> t
    val ( $$ ) : t -> t -> t
    val ( <|> ) : t -> t -> t
    val ( <+> ) : t -> t -> t
    val ( </> ) : t -> t -> t
    val ( <//> ) : t -> t -> t
  end

  module Characters : sig
    val qmark : t
    val bang : t
    val at : t
    val sharp : t
    val dollar : t
    val percent : t
    val caret : t
    val ampersand : t
    val star : t
    val comma : t
    val dot : t
    val bar : t
    val colon : t
    val scolon : t
    val equals : t
    val plus : t
    val minus : t
    val underscore : t
    val tilde : t
    val squote : t
    val dquote : t
    val bquote : t
    val slash : t
    val bslash : t
    val lt : t
    val gt : t
    val lbrack : t
    val rbrack : t
    val lbrace : t
    val rbrace : t
    val lparen : t
    val rparen : t
    val space : t
  end
end

module Make (W : Width) : S = struct
  type t = MeasureText.t list

  include W

  let pareto =
    let rec go acc = function
      | [] -> acc
      | x :: xs ->
        if List.exists acc ~f:(fun y -> MeasureText.(y << x)) then go acc xs
        else go (x :: List.filter acc ~f:(fun y -> not MeasureText.(x << y))) xs
    in
    go []

  let ( <> ) xs ys =
    List.cartesian_product xs ys
    |> List.filter_map ~f:(fun (x, y) ->
           MeasureText.(
             let xy = x <> y in
             Option.some_if (valid W.width xy) xy))
    |> pareto

  let flush xs = pareto (List.map ~f:MeasureText.flush xs)
  let text s = List.filter ~f:(MeasureText.valid W.width) [ MeasureText.text s ]

  let render xs =
    List.min_elt xs ~compare:MeasureText.compare
    |> Option.map ~f:MeasureText.render

  let fail = []
  let choice xss = List.concat xss |> pareto
  let empty = text ""
  let nest n x = text (replicate_char n ' ') <> x

  module Infix = struct
    let ( !^ ) = text
    let ( <> ) = ( <> )
    let ( $$ ) a b = flush a <> b
    let ( <|> ) xs ys = choice [ xs; ys ]
    let ( <+> ) x y = x <> text " " <> y
    let ( </> ) a b = choice [ a <+> b; a $$ b ]
    let ( <//> ) a b = choice [ a <+> b; a $$ nest 2 b ]
  end

  open Infix

  let fold f = function
    | [] -> empty
    | x :: xs -> List.fold ~init:x ~f xs

  let vcat = fold ( $$ )
  let hcat = fold ( <> )
  let hsep = fold ( <+> )

  let sep = function
    | [] -> empty
    | xs -> hsep xs <|> vcat xs

  let intersperse ~sep =
    let rec go acc = function
      | [] -> List.rev acc
      | [ x ] -> go (x :: acc) []
      | x :: xs -> go ((x <> sep) :: acc) xs
    in
    go []

  let intersperse_map ~f ~sep =
    let rec go acc = function
      | [] -> List.rev acc
      | [ x ] -> go (f x :: acc) []
      | x :: xs -> go ((f x <> sep) :: acc) xs
    in
    go []

  module Characters = struct
    let qmark = text "?"
    let bang = text "!"
    let at = text "@"
    let sharp = text "#"
    let dollar = text "$"
    let percent = text "%"
    let caret = text "^"
    let ampersand = text "&"
    let star = text "*"
    let comma = text ","
    let dot = text "."
    let bar = text "|"
    let colon = text ":"
    let scolon = text ";"
    let equals = text "="
    let plus = text "+"
    let minus = text "-"
    let underscore = text "_"
    let tilde = text "~"
    let squote = text "'"
    let dquote = text "\""
    let bquote = text "`"
    let slash = text "/"
    let bslash = text "\\"
    let lt = text "<"
    let gt = text ">"
    let lbrack = text "["
    let rbrack = text "]"
    let lbrace = text "{"
    let rbrace = text "}"
    let lparen = text "("
    let rparen = text ")"
    let space = text " "
  end
end
