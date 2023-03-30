type tree = { c : int; u : int; l : tree; r : tree }
type ident = Upper of string | Lower of string
type const = Int of int | Char of char | String of string | Float of float

type t1 =
  | Ident of ident
  | Const of const
  | Sym of string
  | Parens of t1
  | Braces of t1
  | Brackets of t1
  | Semi of t1 * t1
  | Comma of t1 * t1
  | Juxt of t1 * t1

type t2 =
  | Lit of const
  | Sym of string
  | Two of { tag : int; one : t2; two : t2 }
  (* [0 ] [1 ;] [2 ,] *)
  | One of { tag : int; one : t2 }
  (* [0 ] [1 ()] [2 {}] [3 []] *)
  | Shape of string * t2 * t2 list

module Shape_call = struct
  type syntax =
    | Const of const
    | Sym of string
    | Scope of string * syntax * string
    | Seq of string option * syntax list
    | Hole
    | Shape of string * syntax
    | Meta of syntax * syntax

  (* Smart constructors *)
  let lower x = Ident (Lower x)
  let upper x = Ident (Upper x)
  let sym ?meta:_ x = Sym x
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
  let shape name syn = Shape (name, syn)
  let __ = Hole

  (* Examples *)

  let mul_shape = Shape ("mul", seq [ __; sym ~meta:(int 20) "*"; __ ])
  let add_shape = Shape ("add", seq [ __; sym ~meta:(int 30) "+"; __ ])
  let x1 = seq [ add_shape; int 2; int 3 ]
  let x2 = seq [ mul_shape; x1; int 0 ]

  (* Seq [Shape _; ...] is a macro call *)

  let _ =
    match x2 with
    | Seq (None, [ Shape ("mul", _); _; Const (Int 0) ])
    | Seq (None, [ Shape ("mul", _); Const (Int 0); _ ]) -> Const (Int 0)
    | x -> x

  (* This seems to work fine, but requires direct data. Let's turn shapes into
     GADTs indexed by hole types. *)
end

(* Indexed typed syntax *)
module Stydx = struct
  type ('h, 'r) t =
    | Hole_int : (int, 'r) t
    | Text : string -> ('h, 'r) t
    | Cons : ('h1, 'r) t * ('h2, 'r) t -> ('h1 -> 'h2 -> 'r, 'r) t
    | Shape : string * ('h, 'r) t -> ('h, 'r) t

  let mul_shape = Shape ("mul", Cons (Hole_int, Cons (Text "*", Hole_int)))
end

(* Syntax based on generics encoding. *)
module Syngen = struct
  type _ h = Unit : unit h | Int : int h | Text : string -> string h

  type ('h, 'r) t1 =
    | [] : ('r, 'r) t1
    | ( :: ) : 'a h * ('h, 'r) t1 -> ('a -> 'h, 'r) t1

  let x1 = [ Int; Text "+"; Int ]

  type ('h, 'r) t2 =
    | End : ('r, 'r) t2
    | Infix : 'h1 h * string * 'h2 h -> ('h1 -> 'h2, 'r) t2

  let x1 = Infix (Int, "+", Int)
end

module Shape_quote = struct
  type shape =
    | Const of const
    | Sym of string
    | Scope of string * shape * string
    | Seq of string option * shape list
    | Splice of shape
    | Quote of shape

  type node = { meta : shape; data : shape; span : int * int * int * int }

  (* Smart constructors *)
  let lower x = Ident (Lower x)
  let upper x = Ident (Upper x)
  let sym ?meta:_ x = Sym x
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
end

module Shape_intern = struct
  type 'a cons = { i : int; e : int; l : 'a cons; r : 'a cons }

  let rec null = { i = 0; e = 0; l = null; r = null }
  let meta m x = { i = 1; e = 1; l = m; r = x }
  let quote x = { i = 2; e = 2; l = x; r = null }
  let unquote x = { i = 3; e = 3; l = x; r = null }
end

module Fl_shapes = struct
  let sym _ = ()
  let seq _ = ()
  let lower _ = ()
  let arrow _ = ()
  let brackets _ = ()
  let hole = sym "_"
  let __ = sym "_"
  let ( !! ) _ = ()

  let if_else =
    !!(seq [ lower "if"; __; brackets __; lower "else"; brackets __ ])

  (* `(,a -> ,b) *)
  let arrow = !!(seq [ __; sym "->"; __ ])
  let binding = !!(seq [ __; sym "="; __ ])
  let constraint' = !!(seq [ __; sym ":"; __ ])
  let fn = !!(seq [ sym "fn"; seq [ __ ]; sym "->"; __ ])
  let fn_case = !!(seq [ sym "fn"; seq [ __ ]; sym "->"; __ ])
  let case = !!(seq [ sym "|" ])

  module Mk = struct
    let arrow a b = seq [ arrow; a b ]
  end
end
