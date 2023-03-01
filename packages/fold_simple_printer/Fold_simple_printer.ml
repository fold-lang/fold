let parse_structure input =
  let lexbuf = Lexing.from_channel input in
  let next_token = Lexer.token in
  let structure = Parser.implementation next_token lexbuf in
  structure

let () =
  let chan = try open_in Sys.argv.(1) with _ -> stdin in
  let structure = parse_structure chan in
  let syn = Fold_syntax.Syntax_encoder.structure structure in
  (* Fmt.pr "%a@." Fold_syntax.Syntax.pp syn *)
  Fold_pprint.print syn
