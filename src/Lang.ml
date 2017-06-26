

(* let default_operator token = *)
(*   token *)
(*   |> Precedence.lookup *)
(*   |> Option.map (fun precedence -> *)
(*       let parse x = *)
(*         Parser.advance >>= fun () -> *)
(*         parse_prefix precedence >>= fun y -> *)
(*         Parser.pure (`Form [(token :> Syntax.t); x; y]) in *)
(*       (parse, precedence)) *)


(* let define_syntax_operators grammar = *)
(*   grammar *)
(*   |> postfix 70 "*" (fun x   -> Syntax.form [`Symbol "*"; x]) *)
(*   |> postfix 70 "+" (fun x   -> Syntax.form [`Symbol "+"; x]) *)
(*   |> postfix 70 "?" (fun x   -> Syntax.form [`Symbol "?"; x]) *)
(*   |> infix   30 "|" (fun x y -> Syntax.form [`Symbol "|"; x; y]) *)


(* let define_syntax = *)
(*   let parse = *)
(*     Parser.modify begin fun state -> *)
(*       State.{ state with grammar = *)
(*                 define_syntax_operators (Grammar.new_scope state.grammar) } *)
(*     end >>= fun () -> *)
(*     Parser.advance >>= fun () -> *)
(*     expression >>= fun x -> *)
(*     Parser.modify begin fun state -> *)
(*       State.{ state with grammar = Grammar.pop_scope state.grammar } *)
(*     end >>= fun () -> Parser.pure (`Form [`Symbol "syntax"; x]) in *)
(*   Grammar.define_prefix (`Symbol "syntax") parse *)

(* let grammar = *)
  (* Grammar.init *)
    (* ~atom:(fun x -> x :> AST.t) *)
    (* ~form:(fun x -> default_operator x or lazy (juxtaposition x)) *)
    (* () *)
  (* |> define_syntax *)

  (* |> val_syntax *)
  (* |> delimiter "=" *)

  (* |> between "(" ")" (fun x -> x) *)

