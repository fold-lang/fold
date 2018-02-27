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

    let parse g =
      let left =
        P.some begin
          P.current >>= fun token ->
          P.guard (not (P.Grammar.has_left token g)) >>= fun () ->
          P.nud 0 g
        end >>= fun (x, xs) ->
        if List.length xs = 0 then
          P.return x
        else
          P.return (Eval.Expression.apply x xs) in
      left >>= P.led 0 g

    let parse_let_binding g =
      Fmt.pr ">>> parse_let_bindings@.";
      Pattern.parser >>= fun p ->
      Fmt.pr ">>> $p@.";
      symbol "=" >>= fun () ->
      Fmt.pr ">>> `=`@.";
      parse g >>= fun e ->
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
      parse g >>= fun body ->
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
      parse g >>= fun e ->
      Fmt.pr ">>> Expression.parse_group: $e = %a@." Pprintast.expression e;
      symbol ")" >>= fun () ->
      Fmt.pr ">>> Expression.parse_group: `)`@.";
      return e

    let parse_token _g =
      P.current >>= fun t ->
      P.advance >>= fun () ->
      P.return (Eval.Expression.token t)


    let parse_binary f g a =
      P.advance >>= fun () ->
      parse g >>= fun b ->
      P.return (f a b)

    let grammar = P.grammar [
        P.term parse_token;

        P.null (`Symbol "let") parse_let;

        P.left 30 (`Symbol "+")
          (parse_binary (fun a b -> Eval.Expression.(apply (token (`Symbol "+")) [a; b])));

        (* Isn't strictly part of this grammar but needed to stop parsing. *)
        (* Should not be needed *)
        P.delimiter (`Symbol "val");
        P.delimiter (`Symbol "def");
        P.delimiter (`Symbol "in");

        (* Recursive call to parse uses the current `g` potentially updated in RT. *)
        P.null (`Symbol "(") parse_group;

        P.delimiter (`Symbol ")");
      ]

    let parser =
      parse grammar
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

  let parser =
    let left =
      P.some Statement.parser >>= fun (x, xs) ->
      if List.length xs = 0 then
        P.return [x]
      else
        P.return (x :: xs) in
    left >>= P.led 0 Statement.grammar
end

