module P = Shaper_parser.Parser
module L = Shaper_parser.Lexer

let g = Shaper_xml.grammar

let test input =
  try
    let l = L.for_string input in
    let x = P.run g l in
    Fmt.pr "%a@." Shaper_xml.pp x
  with Failure err -> Fmt.pr "err: %s@." err

let () =
  print_endline "\n[basic]";
  test {|"hello"|};
  test {|<p>"hello"</p>|};
  test {|<p> <b> "hello" </b> </p>|};
  test {|<p> <b> "hello" </b> "world" </p>|};
  test {|<p> </p>|}
