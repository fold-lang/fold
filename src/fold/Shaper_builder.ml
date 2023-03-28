type t = Shaper.shape
type ident = Shaper.ident

open struct
  let form = Shaper.form
  let seq = Shaper.seq
  let parens = Shaper.parens
  let braces = Shaper.braces
  let brackets = Shaper.brackets
  let seq_comma = Shaper.seq_comma
  let seq_semi = Shaper.seq_semi
end

(* Ident *)
let lower = Shaper.lower
let upper = Shaper.upper

let longident lid =
  List.fold_left
    (fun acc id ->
      match id.[0] with
      | 'a' .. 'z' -> lower id :: acc
      | 'A' .. 'Z' -> upper id :: acc
      | _ -> Shaper.sym id :: acc
    )
    [] (Longident.flatten lid)
  |> Shaper.form "."

(* Const *)

let string = Shaper.string
let int = Shaper.int
let float = Shaper.float
let char = Shaper.char

let const (const : Parsetree.constant) =
  match const with
  | Pconst_char x -> char x
  | Pconst_float (x, _) -> float (float_of_string x)
  | Pconst_string (x, _, _) -> string x
  | Pconst_integer (x, _) -> int (int_of_string x)

(* f a b *)
let apply f args = seq (f :: args)

(* X a *)
let constr upper_id args = seq (upper upper_id :: args)

(* (), (a, b, c) *)
let tuple items = parens (seq_comma items)

(* {}, {a}, {a, b, c} *)
let array items = braces (seq_comma items)

(* {a;} {a; b; c} *)
let block items = braces (seq_semi items)

(* [a, b, ..c] *)
let list ?spread items =
  match spread with
  | Some tl -> brackets (seq_comma (List.append items tl))
  | None -> brackets (seq_comma items)

(* {a = 1, ~b, ..c} *)
let record ?spread items =
  match spread with
  | Some tl -> form "record" (List.append items tl)
  | None -> form "record" items

(* ..a *)
let spread x = form ".." [ x ]

(* a | b | ... *)
let alt items = form "|" items

(* a.b *)
let dot a b = form "." [ a; b ]

(* a = b *)
let binding a b = form "=" [ a; b ]

(* a -> b *)
let arrow a b = form "->" [ a; b ]

(* fn ... -> ... *)
(* fn { ... } *)
let fn x = form "fn" [ x ]

(* a : b *)
let constraint_ a b = form ":" [ a; b ]

(* f a b *)
let apply f args = seq (f :: args)

(* open M *)
(* open { ... } *)
let open_ mexp = form "open" [ mexp ]

(* let a = 1; ... *)
(* let a = 1, b = 2; ... *)
let let_ vbl = form "let" [ vbl ]
let let_rec vbl = form "let rec" [ vbl ]

(* if a then b else c *)
let if_then_else cond if_true if_false = form "if" [ cond; if_true; if_false ]

(* if a then b *)
let if_then cond if_true = form "if" [ cond; if_true ]

(* match a { ... } *)
let match_ arg = form "match" [ arg ]

(* module M = ... *)
let module_ mb = form "module" [ mb ]

(* `a` *)
let quote x = form "quote" [ x ]

(* $a *)
let splice x = form "$" [ x ]

(* Meta representation of syntax. *)
let rec meta (shape : Shaper.shape) =
  match shape with
  | Ident (Upper id) -> constr "Ident" [ constr "Upper" [ string id ] ]
  | Ident (Lower id) -> constr "Ident" [ constr "Lower" [ string id ] ]
  | Const (Int x) -> constr "Const" [ constr "Int" [ int x ] ]
  | Const (Float x) -> constr "Const" [ constr "Float" [ float x ] ]
  | Const (Char x) -> constr "Const" [ constr "Char" [ char x ] ]
  | Const (String x) -> constr "Const" [ constr "String" [ string x ] ]
  | Sym x -> constr "Sym" [ string x ]
  | Seq (None, items) ->
    constr "Seq" [ constr "None" []; list (List.map meta items) ]
  | Seq (Some sep, items) ->
    constr "Seq" [ constr "Some" [ string sep ]; list (List.map meta items) ]
  | Form ("$", [ code ]) -> code
  | Form (kwd, items) ->
    constr "Form" [ string kwd; list (List.map meta items) ]
  | Scope (l, x, r) -> constr "Scope" [ string l; meta x; string r ]
