module S = Shaper.V03
module P = Shaper_parser.Parser
module G = Shaper_parser.Grammar
module L = Shaper_parser.Lexer

let g = Shaper_fold.g

let item input =
  try
    let l = L.for_string input in
    let fl = P.run g l in
    Fmt.epr "item: fl: @[%a@]@." S.dump fl;
    let ml = Fold_syntax.Conv_fl_to_ml.structure fl in
    Fmt.pr "item: ml: @[%a@." Pprintast.structure ml
  with Failure err -> Fmt.pr "item: err: %s@." err

let exp input =
  try
    let l = L.for_string input in
    let fl = P.run g l in
    Fmt.epr "exp: fl: @[%a@]@." S.dump fl;
    let ml = Fold_syntax.Conv_fl_to_ml.Exp.conv fl in
    Fmt.pr "exp: ml: @[%a@]@." Pprintast.expression ml
  with Failure err -> Fmt.pr "exp: err: %s@." err

let test_item () =
  begin
    item {|val x = 1|};
    item {|
    val a = 1;
    val b = 2
  |}
  end

let test_exp () =
  exp {|let a = 1; a|};
  exp {|let a = 1, b = 2; b|};
  exp {|
    let a = 1;
    let b = 2;
    a + b
  |}

let () =
  begin
    test_exp ();
    test_item ()
  end
