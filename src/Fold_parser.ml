open Local
open Lex

module Make
    (* (Eval : Interpreter.Self) *)
= struct
  module Eval = OCaml

  type expression = Eval.Expression.t
  type pattern = Eval.Pattern.t

  module P = Pratt.Make(Token)
  let (>>=)  = P.(>>=)
  let return = P.return

  let run = P.run

  let symbol s = P.consume (`Symbol s)

  let sep1 ~by:s p =
    p >>= fun x ->
    P.many (s >>= fun () -> p) >>= fun xs ->
    P.return (x :: xs)

  module rec Expression : sig
    val parse_let : expression P.grammar -> expression P.parser
    val grammar : expression P.grammar
    val parser : expression P.parser
  end = struct
    let parse_let_binding g =
      Fmt.pr ">>> parse_let_bindings@.";
      Pattern.parser >>= fun p ->
      Fmt.pr ">>> $p@.";
      symbol "=" >>= fun () ->
      Fmt.pr ">>> `=`@.";
      P.parse g >>= fun e ->
      Fmt.pr ">>> $e@.";
      P.return (p, e)

    let parse_let g =
      Fmt.pr ">>> parse_let@.";
      symbol "let" >>= fun () ->
      Fmt.pr ">>> `let`@.";
      parse_let_binding g >>= fun lb ->
      Fmt.pr ">>> $lb@.";
      symbol "in" >>= fun () ->
      Fmt.pr ">>> `in`@.";
      P.parse g >>= fun body ->
      Fmt.pr ">>> $body@.";
      P.return (Eval.Expression.let' [lb] body)

    let parse_token _g =
      P.next >>= fun t ->
      Fmt.pr ">>> Expression.parse_token: %a@." Token.pp t;
      return (Eval.Expression.token t)

    let parse_group g =
      Fmt.pr ">>> Expression.parse_group@.";
      symbol "(" >>= fun () ->
      Fmt.pr ">>> Expression.parse_group: `(`@.";
      P.parse g >>= fun e ->
      Fmt.pr ">>> Expression.parse_group: $e = %a@." Pprintast.expression e;
      symbol ")" >>= fun () ->
      Fmt.pr ">>> Expression.parse_group: `)`@.";
      return e

    let _parse_token _g =
      P.current >>= fun t ->
      P.advance >>= fun () ->
      P.return (Eval.Expression.token t)

    let parse_term g =
      Fmt.pr ">>> Expression.parse_term@.";
      P.current >>= fun t ->
      P.advance >>= fun () ->
      Fmt.pr ">>> Expression.parse_term: current = %a@." Token.pp t;
      P.parse_many g >>= fun xs ->
      if List.length xs = 0 then
        return (Eval.Expression.token t)
      else
        let f = Eval.Expression.token t in
        return (Eval.Expression.apply f xs)

    let grammar = P.grammar [
        P.term parse_term;

        P.null (`Symbol "let") parse_let;

        P.left 30 (`Symbol "+")
          (P.binary (fun a b -> Eval.Expression.(apply (token (`Symbol "+")) [a; b])));

        (* Isn't strictly part of this grammar but needed to stop parsing. *)
        (* Should not be needed *)
        P.delimiter (`Symbol "val");
        P.delimiter (`Symbol "def");

        (* Recursive call to parse uses the current `g` potentially updated in RT. *)
        P.null (`Symbol "(") parse_group;

        P.delimiter (`Symbol ")");
      ]

    let parser =
      P.parse grammar
  end

  and Pattern : sig
    val parse_constructor : Eval.Pattern.t P.grammar -> Eval.Pattern.t P.parser
    val grammar : Eval.Pattern.t P.grammar
    val parser : Eval.Pattern.t P.parser
  end = struct
    let parse_constructor g =
      (* FIXME: X X X X *)
      P.next >>= function
      | `Symbol x when Char.Ascii.is_upper (String.get x 0) ->
        let is_infix token = P.Grammar.has_left token g in
        P.many_while (not << is_infix) (P.parse g) >>= fun xs ->
        let open Eval in
        let name = Name.id x in
        let args = if xs == [] then None else Some (Pattern.tuple xs) in
        return (Pattern.constructor name args)
      | token -> return (Eval.Pattern.token token)

    let grammar = P.grammar [
        P.term parse_constructor;
        P.between (`Symbol "(") (`Symbol ")") identity;
        P.delimiter (`Symbol ")");
        P.delimiter (`Symbol "=");
      ]
    let parser = P.parse grammar
  end

  module Statement = struct
    let parse_val g =
      Fmt.pr ">>> parse_val@.";
      P.consume (`Symbol "val") >>= fun () ->
      Fmt.pr ">>> `val`@.";
      Pattern.parser >>= fun p ->
      Fmt.pr ">>> $p@.";
      P.consume (`Symbol "=") >>= fun () ->
      Fmt.pr ">>> `=`@.";
      Expression.parser >>= fun e ->
      Fmt.pr ">>> $e@.";
      return (Eval.Statement.val' p e)

    let grammar = P.grammar [
        P.null (`Symbol "val") parse_val;
      ]
    let parser =
      P.current >>= fun t ->
      Fmt.pr ">>> Statement.parser: %a@." Token.pp t;
      P.parse grammar
  end
end

(*
let default_operator token f =
  let open Pratt in
  token
  |> Precedence.lookup
  |> Option.map (fun precedence ->
      let parse g x =
        advance >>= fun () ->
        nud g precedence >>= fun y ->
        pure (f x y) in
      (parse, precedence))


let invalid_keyword token =
  let open Pratt in
  let parse g =
    let msg = "unexpecetd keyword: %s" % Token.to_string token in
    (error (With_message msg)) in
  Prefix (token, parse)

let invalid g left = Pratt.(error (With_message "invalid"))

let keywords () = let open Pratt in [
  invalid_keyword (`Symbol "val");
  invalid_keyword (`Symbol "def");

  Infix (`Symbol "val", (invalid, 0));
  Infix (`Symbol "def", (invalid, 0));

  Prefix (Lex.eof, (const  (error (With_message "unexpected end of file"))));
  Infix  (Lex.eof, (const2 (error Empty), 0));
]

module Make (Eval : Interpreter.Self) = struct

  module Expression = struct
    open Pratt

    let atom token =
      singleton (Eval.Expression.token token)

    let form token =
      let apply x y = Eval.Expression.apply (Eval.Expression.token token) [x; y] in
      default_operator token apply

    let join xs =
      Eval.Expression.apply (List.hd xs) (List.tl xs)

    let grammar =
      Grammar.init ~atom ~form ~join "Expression" (keywords ())

    let parse = Pratt.parse grammar
  end


  module Pattern = struct
    open Pratt

    let atom token = singleton (Eval.Pattern.token token)

    let form (token : Token.t) =
      let apply x y =
        let name = match token with
          | `Symbol name -> Eval.Name.id name
          | _ -> fail "invalid constructor" in
        let args = Eval.Pattern.tuple [x; y] in
        Eval.Pattern.constructor name (Some args) in
      default_operator token apply

    let join xs =
      fail "invalid join"

    let grammar =
      let rules = [
        delimiter "=";
      ] ++ keywords () in
      Grammar.init ~atom ~form ~join "Pattern" rules

    let parse = Pratt.parse grammar
  end


  module Statement = struct
    let val' g =
      let open Pratt in
      consume (`Symbol "val") >>= fun () ->
      Pattern.parse >>= fun pattern ->
      consume (`Symbol "=") >>= fun () ->
      Expression.parse >>= fun value ->
      pure (Eval.Statement.val' pattern value)

    let def g =
      let open Pratt in
      consume (`Symbol "def") >>= fun () ->
      (* XXX: Not a pattern *)
      Pattern.parse >>= fun pattern ->
      consume (`Symbol "=") >>= fun () ->
      Expression.parse >>= fun value ->
      pure (Eval.Statement.def pattern value)

    let grammar =
      let open Pratt in
      Grammar.init "Statement" [
        Prefix (Lex.eof, (const (Pratt.error Empty)));
        Prefix (`Symbol "val", val');
        Prefix (`Symbol "def", def);

        Infix (`Symbol "val", (invalid, 0));
        Infix (`Symbol "def", (invalid, 0));

        Infix  (Lex.eof, (const2 (error Empty), 0));
      ]

    let parse : Eval.Statement.t Pratt.parser =
      Pratt.parse grammar
  end
end

*)
