let () =
  let lexer = Lexing.from_channel stdin in
  let root = Parser.root Lexer.process_token lexer in
  Fmt.pr "%a@." Shaper.pp root
