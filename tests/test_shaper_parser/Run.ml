let rp str =
  try
    let syn = Shaper_parser.parse_string str in
    Fmt.pr "%a@." Shaper.V03.pp_verbose syn
  with Failure err -> Fmt.pr "err: %s@." err

let test_basic () =
  begin
    Fmt.pr "[basic]@.";
    rp "1";
    rp "123456789";
    rp "{}";
    rp "{1}";
    rp "{1 2}";
    rp "{1 2 3}";
    rp "{{}}";
    rp "{{{}}}";
    rp "{1 {}}";
    rp "{1 {} 3}";
    rp "{1 {} {1 2 3}}";
    rp "{{{{{1}}}}}";
    rp "{{0} {1 2} {3 4 5} {6 7 8 9}}"
  end

let test_juxt () =
  begin
    Fmt.pr "[juxt]@.";
    rp "1";
    rp "1 2";
    rp "1 2 3";
    rp "1 2 3 4 5 6 7"
  end

let test_comma () =
  begin
    Fmt.pr "[comma]@.";
    rp "1";
    rp "1,2";
    rp "1,2,3";
    rp "1,2,3,4,5,6,7"
  end

let test_juxt_comma () =
  begin
    Fmt.pr "[juxt_comma]@.";
    rp "0 1,1";
    rp "1,0 1";
    rp "0 1,0 2";
    rp "1,0 1,0 1 2,0 1 2 3";
    rp "0 1 2 3,0 1 2,0 1,1"
  end

let test_seq_mixed () =
  begin
    Fmt.pr "[seq_mixed]@.";
    rp "1,2;3,4";
    rp "1;2,3;4";
    rp "0 1;2,0 3 4;5,6";
    rp "1,0 2;{0 3 4},0 {1,2;3},{1;2,3}"
  end

let test_whitespace () =
  begin
    Fmt.pr "[whitespace]@.";
    rp " 1";
    rp "1 ";
    rp "    {    1    }    ";
    rp {|    {    1    
    
    }
    |}
  end

let test_err_unbalanced () =
  begin
    Fmt.pr "[err_unbalanced]@.";
    rp "{";
    rp "}";
    rp "{{}";
    rp "{}}"
  end

let test_ident () =
  begin
    Fmt.pr "[ident]@.";
    rp "a";
    rp "a_";
    rp "a1";
    rp "fooBar";
    rp "fooBar_BAZ_001";
    rp "X";
    rp "Xa";
    rp "X_";
    rp "X_fooBar"
  end

let test_complex () =
  begin
    Fmt.pr "[complex]@.";
    rp {|
      let f x = {
        let y = x + 1;
        y - 1
      };
    |}
  end

let () = Printexc.record_backtrace true

let () =
  begin
    (* rp "1," *)
    test_basic ();
    print_newline ();
    test_juxt ();
    print_newline ();
    test_comma ();
    print_newline ();
    test_juxt_comma ();
    print_newline ();
    test_seq_mixed ();
    print_newline ();
    test_whitespace ();
    print_newline ();
    test_err_unbalanced ();
    print_newline ();
    test_ident ()
    (* print_newline ();
       test_complex () *)
  end
