module S = Syntax

let parse_ml input =
  let lexbuf = Lexing.from_string input in
  let next_token = Lexer.token in
  let expr = Parser.parse_expression next_token lexbuf in
  expr

let input =
  {|
let rec x = 2 and y = (3, 'a') in
let person = { p with name = "Xavier"; age; } in
Person.print person;
let result = f ~a ?opt ?use:foo ~name:"hello" x in
x + y - (let z = 1 in z * 2)
|}

(*

*)

let () =
  let x = parse_ml input in
  let syn = Conv.conv_exp x in
  Fmt.pr "%a@." (Pp.pp true) syn
