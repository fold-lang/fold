let f01 a = a
let f02 a b = a
let f03 a b c = a
let f04 ~a = a
let f05 ~a:b = a
let f06 ~(a : t) = a
let f07 ~a:(b : t) = a
let f08 ?a = a
let f09 ?a:b = a
let f10 ?(a = 1) = a
let f11 ?a:(b = 1) = a
let f12 ?(a : t = 42) = a
let f13 ?a:(x : t = 42) = a
let a01 = f ~a
let a02 = f ~a:b
let a03 = f ~(a : t)
let a04 = f ~a:(b : t)
let a05 = f ?a
let a06 = f ?a:b
let a07 = f ?a:(a : t) (* not punned in ocaml? *)
let a08 = f ?a:(b : t)
