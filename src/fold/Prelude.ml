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
module Ml = Ppxlib.Parsetree
module Loc = Ppxlib.Location
module Ident = Ppxlib.Longident
module Asttypes = Ppxlib.Asttypes

type loc = Ppxlib.Location.t
type 'a with_loc = 'a Ppxlib.Location.loc
type ident = Ppxlib.Longident.t
type constant = Ppxlib.Parsetree.constant

let noloc = Ppxlib.Location.none
let with_noloc txt = { Ppxlib.Location.txt; loc = Ppxlib.Location.none }
let with_loc loc txt = { Ppxlib.Location.txt; loc }
