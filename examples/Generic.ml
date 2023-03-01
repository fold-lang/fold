module M1 = struct
  let main =
    let rec x = 2 and y = (3, 'a') in
    let person = { p with name = String.capitalize_ascii "xavier"; age; } in
    Person.print person;
    List.iter (fun ~l:m ?a:(x = 1) ?b ?c:d foo -> begin print_endline "foo"; a end) items;
    let result = f ~a ?opt ?use:foo ~name:"hello" x in
    let a = [||], [|1; 2; 3|] in
    let l = [], [1; 2; 3], (::)(1, (::)(2, (::)(3, []))) in
    let l = x1 :: x2 :: xs, x1 :: [x2; x3] in
    let l = (::)(1, (::)(2, (::)(3, tl))) in
    let c = None, Some 1 in
    let c = (Both (1, 2) [@explicit_arity]), Ambos (1, 2) in
    let c = (::) 1, ([])(2, 3) in
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