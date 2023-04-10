module S = Shaper

let item input =
  try
    let fl = Fold.Parser.parse_string input in
    let ml = Fold.To_ocaml.structure fl in
    Fmt.pr "item: ml: @[%a@." Pprintast.structure ml
  with Failure err -> Fmt.pr "item: err: %s@." err

let exp input =
  try
    let fl = Fold.Parser.parse_string input in
    let ml = Fold.To_ocaml.expr fl in
    Fmt.pr "exp: ml: @[%a@]@." Pprintast.expression ml
  with Failure err -> Fmt.pr "exp: err: %s@." err

let test_item () =
  begin
    item {|x = 1|};
    item {|
         a = 1;
         b = 2
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
    exp {|[1 & xs]|};
    exp {|[1, 2 & xs]|};
    exp {|[1 & [2]]|};
    exp {|[1 & [2 & [3, 4]]]|}
  end

let () =
  begin
    test_exp ();
    test_item ();
    test_list ()
  end
