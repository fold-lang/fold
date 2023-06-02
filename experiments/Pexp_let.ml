
// ocaml
let a = 1 in
a + 1

let a = 1 in
let b = a + 1 in
Ok b

let a = 1
and b = 2
in
Ok (a + b)

let rec a = 2 - b
and b = a + 1
in
Ok (a + b)

let x = 42 in
let rec a = 2 - b
and b = a + 1
in
Ok (a + b)


// sexp
// ----

(let [a 1] (+ a 1))

(let
  [a 1]
  [b (+ a 1)]
  (Ok b))

(let
  [a 1
   b 2]
  (Ok (+ a b)))

(let*
  [a (- 2 b)
   b (+ a 1)]
  (Ok (a + b)))


// fold 1
// ------

let a = 1 in
a + 1

let
  a = 1,
  b = a + 1
in
Ok b

let {
  a = 1,
  b = 2
} in
Ok (a + b)

let rec {
  a = 2 - b,
  b = a + 1
} in
Ok (a + b)

let
  x = 42,
  rec {
    a = 2 - b,
    b = a + 1
  }
in
Ok (a + b)


// fold 2
let a = 1;
a + 1

let a = 1;
let b = a + 1;
Ok b

let
  a = 1,
  b = 2;
Ok (a + b)

rec
  a = 2 - b,
  b = a + 1;
Ok (a + b)

let x = 42;
rec
  a = 2 - b,
  b = a + 1;
Ok (a + b)