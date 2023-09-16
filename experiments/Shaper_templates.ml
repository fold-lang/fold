type 'a template =
  (* X *)
  | Atom of string
  (* X a *)
  | Prefix of string * 'a
  (* a X *)
  | Postfix of 'a * string
  (* a X a *)
  | Infix of 'a * string * 'a
  (* a X a X ... a *)
  | Infix_seq of string * 'a list
  (* a X a X ... a X *)
  | Postfix_seq of string * 'a list

(*


   "foo" a

   a "foo"

   a "+" b

   items ","*
*)
