let parse_structure input =
  let lexbuf = Lexing.from_string input in
  let next_token = Lexer.token in
  let structure = Parser.implementation next_token lexbuf in
  structure

let input1 =
  {|
module M1 = struct
  let main =
    let rec x = 2 and y = (3, 'a') in
    let person = { p with name = String.capitalize_ascii "xavier"; age; } in
    Person.print person;
    List.iter (fun ~l:m ?a:(x = 1) ?b ?c:d foo -> begin print_endline "foo"; a end) items;
    let result = f ~a ?opt ?use:foo ~name:"hello" x in
    let a = [||], [|1; 2; 3|] in
    let l = [], [1; 2; 3], (::)(1, (::)(2, (::)(3, []))) in
    let l = x1 :: x2 :: xs in
    let c = None, Some 1 in
    let c = (Both (1, 2) [@explicit_arity]), Ambos (1, 2) in
    let c = (::) 1, ([])(2, 3), (::)(1, (::)(2, (::)(3, "Ceci n'est pas une list"))) in
    x + y - (let z = p.age in z * 2)
end

module M2 = struct end

module M3 = struct
  let main =
    for i = 1 to 10 do
      print_endline "Number: ";
      print_int i;
      print_newline ()
    done;
    print_endline "Done."

  let f x =
    match x with
    | [] -> "zero"
    | [1; 2; 3] -> "some"
    | (::)(1, (::)(2, (::)(3, []))) -> "someish"
    | (::)(1, (::)(2, (::)(3, "nop"))) -> "nop"
end
|}

let input2 =
  {|
let x =
  let y = 1 in
  let y = 2 in
  let y = 3 in
  foo

let x = 2
let x = 3
|}

(*
let rec x = 2, y = {3, 'a'};
let person = { ..p, name = "Xavier", ~age };
Person.print person;
let result = f ~a ?opt ?use:foo ~name:"hello" x in
x + y - (let z = p.age in z * 2)
*)

let () =
  let structure = parse_structure input1 in
  let syn = Fold_syntax.Syntax_encoder.structure structure in
  (* Fmt.pr "OUTPUT:@.%a@." Fold_syntax.Syntax.pp syn *)
  Fold_pprint.print syn
