module V01 = struct
  module Syn = Shaper.V01

  (* [a, b, ..c] *)
  let list ?spread items : 'a Syn.syntax =
    match spread with
    | Some tl -> Syn.Form ("list", List.append items tl)
    | None -> Syn.Form ("list", items)

  (* {a = 1, ~b, ..c} *)
  let record ?spread items : 'a Syn.syntax =
    match spread with
    | Some tl -> Syn.Form ("record", List.append items tl)
    | None -> Syn.Form ("record", items)

  (* (a, b, c) *)
  let tuple items : 'a Syn.syntax = Syn.Form ("tuple", items)

  (* {a, b, c} *)
  let array items : 'a Syn.syntax = Syn.Form ("array", items)

  (* { a; b; c } *)
  let block items : 'a Syn.syntax = Syn.Form ("block", items)

  (* ..a *)
  let spread x : 'a Syn.syntax = Syn.Form ("..", [ x ])

  (* a -> b *)
  let arrow a b : 'a Syn.syntax = Syn.Form ("->", [ a; b ])

  (* a : b *)
  let constraint_ a b : 'a Syn.syntax = Syn.Form (":", [ a; b ])

  (* a | b *)
  let alt a b : 'a Syn.syntax = Syn.Form ("|", [ a; b ])

  (* a.b *)
  let field a b : 'a Syn.syntax = Syn.Form (".", [ a; b ])

  (* a = b *)
  let binding a b : 'a Syn.syntax = Syn.Form ("=", [ a; b ])

  (* fn a b -> c *)
  let fn args body : 'a Syn.syntax = arrow (Syn.Form ("fn", args)) body

  (* open M *)
  let open_ mexp : 'a Syn.syntax = Syn.Form ("open", [ mexp ])

  (* f a b *)
  let apply f args : 'a Syn.syntax = Syn.Call (f, args)

  (* module Mb *)
  let module_ mb : 'a Syn.syntax = Syn.Form ("module", [ mb ])
end

module V03 = struct
  module Syn = Shaper.V03

  (* (), (a, b, c) *)
  let tuple items = Syn.parens (Syn.seq_comma items)

  (* {}, {a}, {a, b, c} *)
  let array items = Syn.braces (Syn.seq_comma items)

  (* {a;} {a; b; c} *)
  let block items = Syn.braces (Syn.seq_semi items)

  (* [a, b, ..c] *)
  let list ?spread items =
    match spread with
    | Some tl -> Syn.brackets (Syn.seq (List.append items tl))
    | None -> Syn.brackets (Syn.seq items)

  (* {a = 1, ~b, ..c} *)
  let record ?spread items =
    match spread with
    | Some tl -> Syn.Form ("record", List.append items tl)
    | None -> Syn.Form ("record", items)

  (* ..a *)
  let spread x = Syn.Form ("..", [ x ])

  (* a | b *)
  let alt a b = Syn.Form ("|", [ a; b ])

  (* a.b *)
  let field a b = Syn.Form (".", [ a; b ])

  (* a = b *)
  let binding a b = Syn.Form ("=", [ a; b ])

  (* a -> b *)
  let arrow a b = Syn.Form ("->", [ a; b ])

  (* (-> (fn arg_1 arg_2) body) *)
  let fn args body = arrow (Syn.Form ("fn", args)) body

  (* a : b *)
  let constraint_ a b = Syn.Form (":", [ a; b ])

  (* f a b *)
  let apply f args = Syn.seq (f :: args)
end
