open Prelude
module Mlb = Ppxlib.Ast_builder.Default

let const_ml_of_fl (const : Shaper.const) =
  match const with
  | Int x -> Mlb.eint x
  | Float x -> Mlb.efloat (string_of_float x)
  | String x -> Mlb.estring x
  | Char x -> Mlb.echar x

let ident_ml_of_fl (ident : Shaper.ident) =
  match ident with
  | Lower x -> Ident.Lident x
  | Upper x -> Ident.Lident x

let ident_of_str id =
  match id.[0] with
  | 'a' .. 'z' -> Shaper.lower id
  | 'A' .. 'Z' -> Shaper.upper id
  | _ -> Shaper.sym id

let ident_fl_of_ml lid =
  match Ident.flatten lid with
  | [ x ] -> ident_of_str x
  | xs ->
    xs
    |> List.fold_left (fun acc id -> ident_of_str id :: acc) []
    |> List.rev
    |> Shaper.shape "."

type t = Shaper.syntax

let pp_ident = Shaper.pp_ident
let pp_const = Shaper.pp_const
let is_scope = Shaper.is_scope

let is_binding = function
  | Shaper.Shape (_, "=", _) -> true
  | _ -> false

module Cons = struct
  include Shaper

  type t = Shaper.syntax

  (* Ident *)
  let lower = Shaper.lower
  let upper = Shaper.upper

  let longident lid =
    match Ident.flatten lid with
    | [ x ] -> ident_of_str x
    | xs ->
      xs
      |> List.fold_left (fun acc id -> ident_of_str id :: acc) []
      |> List.rev
      |> shape "."

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
  let construct ident_ml args = seq (ident_fl_of_ml ident_ml :: args)

  (* (), (a, b, c) *)
  let tuple items = parens (Shaper.comma items)
  let unit = parens (Shaper.comma [])

  (* {}, {a}, {a, b, c} *)
  let array items = braces (Shaper.comma items)

  (* {a;} {a; b; c} *)
  let block items = braces (Shaper.semi items)

  (* ..a *)
  let spread x = shape ".." [ x ]

  (* {a = 1, ~b, ..c} *)
  let record ?spread:x items =
    match x with
    | Some x -> braces (Shaper.comma (List.append items [ spread x ]))
    | None -> braces (Shaper.comma items)

  (* [a, b, ..c] *)
  let list ?spread:x items =
    match x with
    | Some x -> braces (Shaper.comma (List.append items [ spread x ]))
    | None -> brackets (Shaper.comma items)

  (* a | b | ... *)
  let alt items = shape "|" items

  (* a = b *)
  let binding a b = shape "=" [ a; b ]

  (* a -> b *)
  let arrow a b = shape "->" [ a; b ]

  (* [pat -> exp] or [pat if guard -> exp] *)
  let case pat ?guard exp =
    match guard with
    | None -> arrow pat exp
    | Some guard -> arrow (shape "if" [ pat; guard ]) exp

  (* fn ... -> ... *)
  (* fn { ... } *)
  let fn args body = arrow (shape "fn" args) body
  let fn_single x = shape "fn" [ x ]
  let fn_match cases = shape "fn" [ braces (seq cases) ]

  (* a : b *)
  let constraint' a b = shape ":" [ a; b ]

  (* open M *)
  (* open { ... } *)
  let open' mexp = shape "open" [ mexp ]
  let label l v = shape "~" [ lower l; v ]
  let label_opt l v = shape "~?" [ lower l; v ]
  let label_pun l = shape "~" [ lower l ]
  let label_opt_pun l = shape "~?" [ lower l ]

  (* while a do { ... } *)
  let while' cond body = shape "while" [ cond; body ]

  let for' ~down binding to_exp body =
    shape (if down then "for down" else "for") [ binding; to_exp; body ]

  (* a.b *)
  let field exp lid = shape "." [ exp; longident lid ]

  (* a.b *)
  (* TODO: remove *)
  let dot a b = shape "." [ a; b ]
  let let' x = shape "let" [ x ]
  let let_rec x = shape "let rec" [ x ]

  (* if a then b else c *)
  let if_then_else cond if_true if_false =
    shape "if" [ cond; if_true; if_false ]

  (* if a then b *)
  let if_then cond if_true = shape "if" [ cond; if_true ]

  (* match a { (cases,)* } *)
  let match' exp cases = shape "match" [ exp; braces (Shaper.comma cases) ]
  let match_single arg = shape "match" [ arg ]

  (* module M = ... *)
  let module' mb = shape "module" [ mb ]
  let module_rec mbl = shape "module rec" mbl

  (* let quote x = shape "quote" [ x ] *)
  (* let quasiquote x = shape "quasiquote" [ x ] *)
  (* let quasiquote x = seq (lower "quasiquote" :: [x]) *)
  let unquote x = shape "unquote" [ x ]

  (* for all 'a 'b, 'a -> 'b *)
  let for_all vars t = shape "for all" [ Shaper.comma [ seq vars; t ] ]
end

module Cons_next = struct
  type t = Shaper.syntax

  (* Ident *)
  let lower = Shaper.lower
  let upper = Shaper.upper

  let longident lid =
    match Ident.flatten lid with
    | [ x ] -> ident_of_str x
    | xs ->
      xs
      |> List.fold_left (fun acc id -> ident_of_str id :: acc) []
      |> List.rev
      |> Shaper.shape "."

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
  let apply f args = Shaper.seq (f :: args)

  (* X a *)
  let construct ident_ml args = Shaper.seq (ident_fl_of_ml ident_ml :: args)

  (* (), (a, b, c) *)
  let tuple_kwd = Shaper.lower "tuple"
  let tuple items = Shaper.seq (tuple_kwd :: items)

  (* {}, {a}, {a, b, c} *)
  let array_kwd = Shaper.lower "array"
  let array items = Shaper.seq (array_kwd :: items)

  (* {a;} {a; b; c} *)
  let do_kwd = Shaper.lower "do"
  let do' items = Shaper.seq (do_kwd :: items)

  (* {a = 1, ~b, ..c} *)
  let record_kwd = Shaper.lower "record"
end
