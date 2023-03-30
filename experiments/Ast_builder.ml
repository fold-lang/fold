type t = Ast.t

let id = Ast.id
let const x = Ast.Const x
let longident x = Ast.Id x
let comma = id ","
let module_ mb = Ast.Form [ id "module"; mb ]

let module_rec mbl =
  let items =
    match mbl with
    | [] -> []
    | hd :: tl -> List.fold_left (fun acc vb -> vb :: id "and" :: acc) [ hd ] tl
  in
  Ast.Form (id "module" :: id "rec" :: List.rev items)

let mk_let ~prefix vbl =
  let items =
    match vbl with
    | [] -> []
    | hd :: tl -> List.fold_left (fun acc vb -> vb :: id "and" :: acc) [ hd ] tl
  in
  Ast.Form (List.append prefix (List.rev items))

let let_ vbl = mk_let ~prefix:[ id "let" ] vbl
let let_rec vbl = mk_let ~prefix:[ id "let"; id "rec" ] vbl
let val_ vbl = mk_let ~prefix:[ id "val" ] vbl
let val_rec vbl = mk_let ~prefix:[ id "val"; id "rec" ] vbl
let cases items = Ast.Form (id "|" :: items)
let arrow a b = Ast.Form [ id "->"; a; b ]
let constraint_ a b = Ast.Form [ id ":"; a; b ]
let block items = Ast.Block items
let tuple items = Ast.Tuple items
let apply f args = Ast.Apply (f, args)
let label ~optional l v = Ast.Labeled (l, optional, v)
let field exp lid = Ast.Form [ id "."; exp; Id lid ]
let record ?spread:r0 fields = Ast.Record (fields, r0)
let open_ mexp = Ast.Form [ id "open"; mexp ]
let binding exp_1 exp_2 = Ast.Form [ id "="; exp_1; exp_2 ]
let while_ cond body = Ast.Form [ id "while"; cond; id "do"; body ]
let match_ exp cases = Ast.Form [ id "match"; exp; id "with"; cases ]
let if_then cond if_true = Ast.Form [ id "if"; cond; id "then"; if_true ]

let if_then_else cond if_true if_false =
  Ast.Form [ id "if"; cond; id "then"; if_true; id "else"; if_false ]

(* [pat -> exp] or [pat if guard -> exp] *)
let case pat ?guard exp =
  match guard with
  | None -> arrow pat exp
  | Some guard -> arrow (Ast.Form [ pat; id "if"; guard ]) exp

let for_ ?(down = false) binding to_exp body =
  Ast.Form
    [ id "for"
    ; binding
    ; id (if down then "down" else "to")
    ; to_exp
    ; id "do"
    ; body
    ]

let fn args body = arrow (Ast.Form (id "fn" :: args)) body
let array items = Ast.Array items
let list ?spread:tl items = Ast.List (items, tl)

let is_binding = function
  | Ast.Form [ Ast.Id (Lident "="); _; _ ] -> true
  | _ -> false
