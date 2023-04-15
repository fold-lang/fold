let ( <| ) = Stdlib.( @@ )
let ( @@ ) = `disallowed
let ( @ ) = `disallowed
let is = Stdlib.( == )
let ( = ) : int -> int -> bool = Stdlib.( = )
let ( > ) : int -> int -> bool = Stdlib.( > )
let ( < ) : int -> int -> bool = Stdlib.( < )
let ( >= ) : int -> int -> bool = Stdlib.( >= )
let compare : int -> int -> int = Stdlib.compare

type fl = Shaper.syntax
type ml = Parsetree.structure

module List_ext = struct
  let is_empty = function
    | [] -> true
    | _ -> false
end

(* Astlib *)
module Ml = Astlib.Ast_500.Parsetree
module Loc = Astlib.Location
module Ident = Astlib.Longident

type loc = Astlib.Location.t
type 'a with_loc = 'a Astlib.Location.loc
type ident = Astlib.Longident.t
type constant = Astlib.Ast_500.Parsetree.constant

let noloc = Astlib.Location.none
let with_noloc txt = { Astlib.Location.txt; loc = Astlib.Location.none }
let with_loc loc txt = { Astlib.Location.txt; loc }
