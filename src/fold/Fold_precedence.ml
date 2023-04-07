let juxt = 300
let item = 11

let get str =
  match str.[0] with
  | '=' -> 1
  | '<' | '>' -> 2
  | '#' | '&' -> 2
  | '|' -> 2
  | '+' | '-' -> 3
  | '*' | '/' -> 4
  | _ -> 0
