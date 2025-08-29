let ( <| ) = Stdlib.( @@ )
let ( @@ ) = `disallowed
let ( @ ) = `disallowed
let is = Stdlib.( == )
let ( = ) : int -> int -> bool = Stdlib.( = )
let ( > ) : int -> int -> bool = Stdlib.( > )
let ( < ) : int -> int -> bool = Stdlib.( < )
let ( >= ) : int -> int -> bool = Stdlib.( >= )
let compare : int -> int -> int = Stdlib.compare

module List_ext = struct
  let is_empty = function
    | [] -> true
    | _ -> false
end

module Current_ast = Ppxlib_ast.Compiler_version
module Target_ast = Astlib.Ast_500
module Ml = Target_ast.Parsetree
module Asttypes = Target_ast.Asttypes
module Ast_helper = Ppxlib_ast.Ast_helper
module Loc = Ppxlib.Location
module Ident = Ppxlib.Longident

type fl = Shaper.syntax
type ml = Ml.structure

module Conv =
  Ppxlib_ast.Convert
    (Ppxlib_ast__Versions.OCaml_500)
    (Ppxlib_ast.Compiler_version)

type loc = Loc.t
type 'a with_loc = 'a Loc.loc
type ident = Ppxlib.Longident.t
type constant = Ml.constant

let noloc = Loc.none
let with_noloc txt = { Loc.txt; loc = Loc.none }
let with_loc loc txt = { Loc.txt; loc }
