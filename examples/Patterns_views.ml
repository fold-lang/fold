let check_is_blockish = function
  | Syntax.Block _ | Record _ | Tuple _ | Array _ | List _ -> true
  | _ -> false

(* Without view *)
let rec fmt ?(enclose = true) ?(inline = false) (syn : Syntax.t) =
  match syn with
  | Constraint (v, t) when check_is_blockish t ->
    fmt_constraint_blockish ~enclose v t
  | Constraint (v, t) -> fmt_constraint ~enclose v t
  | _ -> P.string "$FMT"

let classify_block = function
  | Syntax.Block _ | Record _ | Tuple _ | Array _ | List _ -> `Block
  | _ -> `Not_block

(* With view *)
let rec fmt ?(enclose = true) ?(inline = false) (syn : Syntax.t) =
  match syn with
  | Constraint (v, check_is_blockish t -> `Block) ->
    fmt_constraint_blockish ~enclose v t
  | Constraint (v, t) -> fmt_constraint ~enclose v t
  | _ -> P.string "$FMT"
