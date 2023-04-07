let ( <| ) = Stdlib.( @@ )
let ( @@ ) = `disallowed
let ( @ ) = `disallowed
let is = Stdlib.( == )
let ( = ) : int -> int -> bool = Stdlib.( = )
let ( > ) : int -> int -> bool = Stdlib.( > )
let ( < ) : int -> int -> bool = Stdlib.( < )
let ( >= ) : int -> int -> bool = Stdlib.( >= )
let compare : int -> int -> int = Stdlib.compare

module Ml = Parsetree
module Ml_cons = Ast_helper

type fl = Shaper.syntax
type ml = Parsetree.structure

module List_ext = struct
  let is_empty = function
    | [] -> true
    | _ -> false
end
