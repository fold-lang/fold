let semi = 10
let item = 11
let ampr = 19
let comma = 20
let equal = 30
let pipe = 40
let arrow = 50
let colon = 60
let excl = 210
let juxt = 300
let dot = 310

let get str =
  match str.[0] with
  | '=' -> 101
  | '<' | '>' -> 102
  | '#' | '&' -> 102
  | '|' -> 102
  | '+' | '-' -> 103
  | '*' | '/' -> 104
  | _ -> 100
