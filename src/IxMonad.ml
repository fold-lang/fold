
type ('a, 'i, 'j) t = 'i -> ('a * 'j)

let pure a = fun i -> (a, i)

let (>>=) m f =
  fun i ->
    let (a, j) = m i in
    let m' = f a in
    m' j

let get   = fun s -> (s,  s)
let put s = fun _ -> ((), s)

