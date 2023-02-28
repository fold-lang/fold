let id = Syntax.id
let comma = id ","
let module_ mb = Syntax.Form [ id "module"; mb ]

let mk_let ~prefix vbl =
  let items =
    match vbl with
    | [] -> []
    | hd :: tl -> List.fold_left (fun acc vb -> vb :: id "and" :: acc) [ hd ] tl
  in
  Syntax.Form (List.append prefix (List.rev items))

let let_ vbl = mk_let ~prefix:[ id "let" ] vbl
let or_ cases = Syntax.Or cases
let arrow a b = Syntax.Arrow (a, b)
let let_rec vbl = mk_let ~prefix:[ id "let"; id "rec" ] vbl
let block items = Syntax.Block items
let tuple items = Syntax.Tuple items
let apply f args = Syntax.Apply (f, args)
let label ~optional l v = Syntax.Labeled (l, optional, v)
let field exp lid = Syntax.Field (exp, lid)
let record r0 fields = Syntax.Record (r0, fields)
let binding exp_1 exp_2 = Syntax.Binding (exp_1, exp_2)
let while_ cond body = Syntax.Form [ id "while"; cond; id "do"; body ]
let match_ exp cases = Syntax.Form [ id "match"; exp; id "with"; cases ]
let if_then cond if_true = Syntax.Form [ id "if"; cond; id "then"; if_true ]

let if_then_else cond if_true if_false =
  Syntax.Form [ id "if"; cond; id "then"; if_true; id "else"; if_false ]

let case pat ?guard exp =
  match guard with
  | None -> arrow pat exp
  | Some guard -> arrow (Syntax.Form [ pat; id "if"; guard ]) exp

let for_ ?(down = false) binding to_exp body =
  Syntax.Form
    [ id "for"
    ; binding
    ; id (if down then "down" else "to")
    ; to_exp
    ; id "do"
    ; body
    ]

let fn args body = Syntax.Fn (args, body)
let array items = Syntax.Array items
let list items = Syntax.List items
