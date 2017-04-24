
open Pratt


let define_delimiter name g =
  g
  |> Grammar.define_prefix name Pratt.invalid_prefix
  |> Grammar.define_infix name (Pratt.invalid_infix, 0)


