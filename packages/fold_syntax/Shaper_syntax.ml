(* [a, b, ..c] *)
let list ?spread items : 'a Shaper.syntax =
  match spread with
  | Some tl -> Shaper.Form ("list", List.append items tl)
  | None -> Shaper.Form ("list", items)

(* {a = 1, ~b, ..c} *)
let record ?spread items : 'a Shaper.syntax =
  match spread with
  | Some tl -> Shaper.Form ("record", List.append items tl)
  | None -> Shaper.Form ("record", items)

(* (a, b, c) *)
let tuple items : 'a Shaper.syntax = Shaper.Form ("tuple", items)

(* {a, b, c} *)
let array items : 'a Shaper.syntax = Shaper.Form ("array", items)

(* { a; b; c } *)
let block items : 'a Shaper.syntax = Shaper.Form ("block", items)

(* ..a *)
let spread x : 'a Shaper.syntax = Shaper.Form ("..", [ x ])

(* a -> b *)
let arrow a b : 'a Shaper.syntax = Shaper.Form ("->", [ a; b ])

(* a : b *)
let constraint_ a b : 'a Shaper.syntax = Shaper.Form (":", [ a; b ])

(* a | b *)
let alt a b : 'a Shaper.syntax = Shaper.Form ("|", [ a; b ])

(* a.b *)
let field a b : 'a Shaper.syntax = Shaper.Form (".", [ a; b ])

(* a = b *)
let binding a b : 'a Shaper.syntax = Shaper.Form ("=", [ a; b ])

(* fn a b -> c *)
let fn args body : 'a Shaper.syntax = arrow (Shaper.Form ("fn", args)) body

(* open M *)
let open_ mexp : 'a Shaper.syntax = Shaper.Form ("open", [ mexp ])

(* f a b *)
let apply f args : 'a Shaper.syntax = Shaper.Call (f, args)

(* module Mb *)
let module_ mb : 'a Shaper.syntax = Shaper.Form ("module", [ mb ])
