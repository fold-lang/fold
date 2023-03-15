let input = {|
var x = if c then a else b
|}

let () =
  let lexer = Lexer.of_string input in
  match Parser.parse lexer with
  | Ok syn -> Fmt.pr "%a@.@." Parser.pp syn
  | Error Zero -> ()
  | Error e -> Fmt.pr "error: %s@." (Parser.error_to_string e)
