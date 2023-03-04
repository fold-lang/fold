let f01 a = a
let f02 a b = a + b
let f03 a b c = a + b + c
let f04 a b c = a + b + c
let f041 aaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ccccccc = a + b + c

let f042 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    c =
  a + b + c

let f042 (aaaaaaaaaaaaaaaaaaaaaaa : aaaaaaaaaaaaaaaaa)
    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb c : float -> unit =
  a + b + c

let f0422 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    cccccccccccccccccccccccccccccccccccccccccccccccccccccc =
  a + b + c

let f043 x : int = x

let f05 = function
  | 0 -> 100
  | n -> n

let f06 t1 t2 =
  match String.compare t1.metric_name t2.metric_name with
  | 0 -> Vendor_device_id.compare t1.device_id t2.device_id
  | ordering -> ordering

let f07 : int -> int -> int = fun t1 t2 -> 0
let f08 t1 t2 : int = 0

let f09 : int -> int option = function
  | 0 -> None
  | x -> Some x


let f10 =
  ( function
    | 0 -> None
    | x -> Some x
    : int -> int option
    )

let f11 =
  List.iter
    (fun x -> let msg = x ^ "!" in prtin_endline msg)
    ["a"; "b"; "c"]

let main () =
  let x = 1 in
  let y =
    let z = 2 in
    2
  in
  x + y

let mk ?loc:_ ?attrs:_ ?docs:_ ?text:_ (str_opt : Ast_helper.str_opt) mexp =
  let mod_name =
    Syntax.id
      ( match str_opt.txt with
      | None -> "_"
      | Some str -> str
      )
  in
  mod_name
