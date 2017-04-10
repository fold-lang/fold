
open Pure
open Fold.Lex

module P = Fold.Parser


let (==>) (p, input) expected =
  let show = Result.show Token.pp Format.pp_print_string in
  let actual = P.parse p input in
  if actual <> expected then begin
    print ("Test failed:\n\t- expected = %s\n\t- actual = %s" %
           (show expected, show actual))
  end


let test () =
  (P.empty, Iter.empty) ==> Error "parsing error: empty";
  (P.exactly (Char 'x'), Iter.of_list [Char 'x']) ==> Ok (Char 'x')


let () =
  test ()

