let id = Syntax.id
let comma = id ","

let rec_module mbl =
  let items =
    match mbl with
    | [] -> []
    | hd :: tl -> List.fold_left (fun acc vb -> vb :: id "and" :: acc) [ hd ] tl
  in
  Syntax.Form (id "module" :: id "rec" :: List.rev items)

let module_ mb = Syntax.Form [ id "module"; mb ]

let mk_let ~prefix vbl =
  let items =
    match vbl with
    | [] -> []
    | hd :: tl -> List.fold_left (fun acc vb -> vb :: id "and" :: acc) [ hd ] tl
  in
  Syntax.Form (List.append prefix (List.rev items))

let let_ vbl = mk_let ~prefix:[ id "let" ] vbl
let let_rec vbl = mk_let ~prefix:[ id "let"; id "rec" ] vbl
let val_ vbl = mk_let ~prefix:[ id "val" ] vbl
let val_rec vbl = mk_let ~prefix:[ id "val"; id "rec" ] vbl
let cases items = Syntax.Form (id "|" :: items)
let arrow a b = Syntax.Form [ id "->"; a; b ]
let constraint_ a b = Syntax.Form [ id ":"; a; b ]
let block items = Syntax.Block items
let tuple items = Syntax.Tuple items
let apply f args = Syntax.Apply (f, args)
let label ~optional l v = Syntax.Labeled (l, optional, v)
let field exp lid = Syntax.Form [ id "."; exp; Id lid ]
let record r0 fields = Syntax.Record (r0, fields)
let open_ mexp = Syntax.Form [ id "open"; mexp ]
let binding exp_1 exp_2 = Syntax.Form [ id "="; exp_1; exp_2 ]
let while_ cond body = Syntax.Form [ id "while"; cond; id "do"; body ]
let match_ exp cases = Syntax.Form [ id "match"; exp; id "with"; cases ]
let if_then cond if_true = Syntax.Form [ id "if"; cond; id "then"; if_true ]

let if_then_else cond if_true if_false =
  Syntax.Form [ id "if"; cond; id "then"; if_true; id "else"; if_false ]

(* [pat -> exp] or [pat if guard -> exp] *)
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

let fn args body = arrow (Syntax.Form (id "fn" :: args)) body
let array items = Syntax.Array items
let list items tl = Syntax.List (items, tl)

let is_binding = function
  | Syntax.Form [ Syntax.Id (Lident "="); _; _ ] -> true
  | _ -> false
