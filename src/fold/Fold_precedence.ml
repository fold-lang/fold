let semi = 10
let item = 11
let ampr = 19
let comma = 20
let equal = 30
let pipe = 40
let on = 40
let colon = 50
let colon_colon = 55
let arrow = 60
let or' = 65
let as' = 70
let excl = 210
let juxt = 300
let dot = 310

(* TODO left/right *)
let get str =
  match str.[0] with
  | '=' -> 101
  | '<' | '>' -> 102
  | '#' | '&' -> 102
  | '|' -> 102
  | '+' | '-' -> 103
  | '*' | '/' -> 104
  | _ -> 100
