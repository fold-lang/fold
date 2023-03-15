module S = Shaper.V03
module P = Shaper_parser.Parser
module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer

let base_g = Shaper_parser.grammar
let calc_g = Shaper_calc.grammar

let calc _g l =
  L.drop (L.Sym "!") l;
  let x = P.parse calc_g l in
  Result.map S.int x

let g = G.def_prefix (L.Sym "!") calc base_g
let input_1 = {|2 + 2|}
let input_2 = {|! ((2 + 2) * 2)|}

let parse_calc input =
  let l = L.for_string input in
  P.run calc_g l

let parse_lang input =
  let l = L.for_string input in
  P.run g l

let rp_lang str =
  try
    let syn = parse_lang str in
    Fmt.pr "lang: %a@." Shaper.V03.pp_verbose syn
  with Failure err -> Fmt.pr "lang: err: %s@." err

let rp_calc str =
  try
    let x = parse_calc str in
    Fmt.pr "calc: %d@." x
  with Failure err -> Fmt.pr "calc: err: %s@." err

let () =
  rp_calc "2 + 2";
  rp_calc "((2 + 2) * 2)"

let () =
  rp_lang "{a, b; c, d}";
  rp_lang input_1;
  rp_lang input_2
