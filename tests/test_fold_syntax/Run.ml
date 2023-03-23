module S = Shaper.V03

let item input =
  try
    let fl = Fold_syntax.Parser.parse_string input in
    let ml = Fold_syntax.To_ocaml.structure fl in
    Fmt.pr "item: ml: @[%a@." Pprintast.structure ml
  with Failure err -> Fmt.pr "item: err: %s@." err

let exp input =
  try
    let fl = Fold_syntax.Parser.parse_string input in
    let ml = Fold_syntax.To_ocaml.Exp.conv fl in
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

let test_list () =
  begin
    (* Simple *)
    exp {|[]|};
    exp {|[1]|};
    exp {|[1, 2]|};
    exp {|[1, 2, 3]|};

    (* Nested *)
    exp {|[[]]|};
    exp {|[[[]]]|};
    exp {|[[1], [2], [], [3, 4], [5]]|};

    (* Spread *)
    exp {|[1, ..xs]|};
    exp {|[1, 2, ..xs]|};
    exp {|[1, ..[2]]|};
    exp {|[1, ..[2, ..[3, 4]]]|}
  end

let () =
  begin
    test_exp ();
    test_item ();
    test_list ()
  end
