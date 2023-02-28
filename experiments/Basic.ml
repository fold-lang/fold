let x = 1
let x = string_of_int x

let x =
  let a = 1 in
  let b = a + 2 in
  Ok (a, b)

type person = { name : string; age : int; profession : string option }

module Person = struct
  let print p = print_endline (p.name ^ " " ^ string_of_int p.age)
end

let p = { name = "Anonymous"; age = 0; profession = None }

let f ~a ?(opt = 0) ?use ~name x =
  ignore (a, opt, use, name, x);
  opt

let x =
  let rec x = 2 and y = (3, 'a') in
  let age = 30 - fst y in
  let person = { p with name = "Xavier"; age } in
  Person.print person;
  let result =
    let a, opt, foo = (1, None, Some (if age > 30 then 0 else 10)) in
    f ~a ?opt ?use:foo ~name:"hello" x
  in
  let rec map : ('a -> 'b) -> 'a list -> 'b list =
   fun f l ->
    match l with
    | [] -> []
    | x :: xs -> f x :: map f xs
  in
  x
  + result
  -
  let z = p.age in
  z * 2
