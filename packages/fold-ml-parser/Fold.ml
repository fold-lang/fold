let () =
  Fold_parser.Lexer.init ();
  let lexbuf = Lexing.from_channel stdin in
  let next_token = Fold_parser.Lexer.token in
  let toplevel_phrases = Fold_parser.Parser.use_file next_token lexbuf in
  let formatter = Format.formatter_of_out_channel stdout in
  List.iter
    (fun tp -> Fold_parser.Printer.top_phrase formatter tp)
    toplevel_phrases;
  ()
