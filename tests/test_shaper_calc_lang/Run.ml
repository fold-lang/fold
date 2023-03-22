module S = Shaper.V03
module P = Shaper_parser.Parser
module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer

let base_g = Shaper_parser.grammar

(* We add core syntax delimiters to allow terminating the inner parser. *)
let calc_g = Shaper_calc.grammar

let form left _g l =
  L.drop (L.Sym "!") l;
  match left with
  | S.Ident (S.Lower "calc") ->
    let x = P.parse_prefix calc_g l in
    Result.map S.int x
  | S.Ident (S.Lower kwd) -> Fmt.failwith "no such macro: %s" kwd
  (* TODO: classify left in error *)
  | _ -> failwith "invalid macro call form, must be kwd!"

let g =
  base_g
  |> G.def (P.Infix (L.Sym "!", (form, 80)))
  |> G.def (P.invalid_prefix_rule (L.Sym "!"))

let rp input =
  try
    let l = L.for_string input in
    let x = P.run g l in
    Fmt.pr "%a@." Shaper.V03.pp x
  with Failure err -> Fmt.pr "err: %s@." err

let () =
  print_endline "\n[no_calc_lang]";
  rp "2";
  rp "2 + 2";
  rp "2 + 2, 2 - 2";
  rp "{a, b; c, d}";

  print_endline "\n[scoping]";
  rp "calc! (2; 1)";
  rp "{calc! (2) + a}";
  rp "calc! 2 + a";
  rp "(calc! 2)";
  rp "(calc! (2))";
  rp "calc! 2 + neg(2)";
  rp "calc! (2 + 3) + pow(2, 4)";

  print_endline "\n[calc_lang]";
  rp "calc! 1";
  rp "calc! 2 + 3";
  rp "calc! 2 + 3, a";
  rp "a, calc! 2 + 3, b";
  rp "a, calc! ((2 + 2) * 2)";
  rp "a, calc! ((2 + 2) * 2), b";
  rp "a { calc! ((2 + 2) * 2) } b";
  rp "a { calc! ((2 + 2) * 2) } b";
  rp "{calc! (2) + 2}";
  rp "{calc! 2 } + 2";
  rp "{calc! 2 + 3 } + 2";
  rp "(f a b, calc! 2 + 3)";

  print_endline "\n[errors]";
  rp "a b calc! 2";
  rp "calc! 2 a";
  rp "calc! 0 )";
  rp "calc! 0 }";
  rp "calc! 0 {";
  rp "calc! (2 + )";
  rp "calc! (2 + ()";
  rp "calc! (2 + 2(";
  rp "calc! (2 + 2}";
  rp "calc! (2 + 2}";

  rp "math! ((2 + 2) * 2)"
