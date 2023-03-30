open Prelude

let const_ml_of_fl (const : Shaper.const) =
  match const with
  | Int x -> Ml_cons.Const.int x
  | Float x -> Ml_cons.Const.float (string_of_float x)
  | String x -> Ml_cons.Const.string x
  | Char x -> Ml_cons.Const.char x

let ident_ml_of_fl (ident : Shaper.ident) =
  match ident with
  | Lower x -> Ml_cons.Exp.ident (Location.mknoloc (Longident.Lident x))
  | Upper x -> Ml_cons.Exp.ident (Location.mknoloc (Longident.Lident x))

let ident_of_str id =
  match id.[0] with
  | 'a' .. 'z' -> Shaper.lower id
  | 'A' .. 'Z' -> Shaper.upper id
  | _ -> Shaper.sym id

let ident_fl_of_ml lid =
  match Longident.flatten lid with
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
  | Shaper.Shape ("=", _) -> true
  | _ -> false

module Cons = struct
  include Shaper

  type t = Shaper.syntax

  (* Ident *)
  let lower = Shaper.lower
  let upper = Shaper.upper

  let longident lid =
    match Longident.flatten lid with
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
  let tuple items = parens (seq_comma items)

  (* {}, {a}, {a, b, c} *)
  let array items = braces (seq_comma items)

  (* {a;} {a; b; c} *)
  let block items = braces (seq_semi items)

  (* ..a *)
  let spread x = shape ".." [ x ]

  (* {a = 1, ~b, ..c} *)
  let record ?spread:x items =
    match x with
    | Some x -> braces (seq_comma (List.append items [ spread x ]))
    | None -> braces (seq_comma items)

  (* [a, b, ..c] *)
  let list ?spread:x items =
    match x with
    | Some x -> braces (seq_comma (List.append items [ spread x ]))
    | None -> brackets (seq_comma items)

  (* a | b | ... *)
  let cases items = shape "|" items

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

  let label ~optional l v =
    if optional then shape "~?" [] else shape "~" [ lower l; v ]

  (* while a do { ... } *)
  let while' cond body = shape "while" [ cond; body ]

  let for' ?(down = false) binding to_exp body =
    shape (if down then "for down" else "for") [ binding; to_exp; body ]

  (* a.b *)
  let field exp lid = shape "." [ exp; longident lid ]

  (* a.b *)
  (* TODO: remove *)
  let dot a b = shape "." [ a; b ]

  (* let a = 1; ... *)
  (* let a = 1, b = 2; ... *)
  let let' vbl = shape "let" vbl
  let let_rec vbl = shape "let rec" vbl
  let val' = let'
  let val_rec = let_rec

  (* if a then b else c *)
  let if_then_else cond if_true if_false =
    shape "if" [ cond; if_true; if_false ]
  (* shape "if_then_else_" [ cond; if_true; if_false ] *)

  (* if a then b *)
  let if_then cond if_true = shape "if" [ cond; if_true ]

  (* match a { ... } *)
  let match' exp cases = shape "match" [ exp; cases ]
  let match_single arg = shape "match" [ arg ]

  (* module M = ... *)
  let module' mb = shape "module" [ mb ]
  let module_rec mbl = shape "module rec" mbl

  (* `a` *)
  let quote x = shape "quote" [ x ]

  (* $a *)
  let splice x = shape "$" [ x ]

  (* for all 'a 'b, 'a -> 'b *)
  let for_all vars t = shape "for all" [ seq_comma [ seq vars; t ] ]
end

module Eval = struct
  type t = Shaper.syntax

  let eval ~const ~ident ~apply ~array ~arrow ~binding ~block ~case ~cases
      ~constraint' ~field ~fn ~fn_match ~for' ~for_all ~if_then ~if_then_else
      ~label ~let' ~let_rec ~list ~match' ~module' ~module_rec ~open' ~record
      ~tuple ~val' ~val_rec ~while' (t : t) =
    match t with
    | Const c -> const const_ml_of_fl
    | Ident id -> ident (ident_ml_of_fl id)
    | Seq (None, f :: args) -> apply f args
    | Scope ("[", Seq (Some ",", items), "]") -> list items
    | Scope ("[", item, "]") -> list [ item ]
    (* match exp cases *)
    | Shape
        ( "match"
        , [ Seq (None, [ exp; Scope ("{", Seq (Some ",", cases), "}") ]) ]
        ) -> match' exp cases
    (* a; b; b *)
    (* | Seq (Some ";", items) -> sequence items *)
    (* Tuple *)
    (* | Scope ("(", Seq (Some ",", []), ")") -> unit *)
    (* fn _ _ _ -> _ *)
    | Shape ("fn", [ Shape ("->", [ Seq (None, args); body ]) ]) -> fn args body
    (* fn _ -> _ *)
    | Shape ("fn", [ Shape ("->", [ arg; body ]) ]) -> fn [ arg ] body
    (* fn { _ -> _ } *)
    | Shape ("fn", [ Scope ("{", Shape ("->", [ arg; body ]), "}") ]) ->
      fn [ arg ] body
    (* fn { _ -> _ | _ -> _ } *)
    | Shape ("fn", [ Scope ("{", Shape ("|", cases), "}") ]) -> fn_match cases
end
