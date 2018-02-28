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
    val parser : expression P.parser
  end = struct
    let parse g =
      let left =
        P.some (P.nud 0 g) >>= fun (x, xs) ->
        if List.length xs = 0 then
          P.return x
        else
          P.return (Eval.Expression.apply x xs) in
      left >>= P.led 0 g

    let parse_let_binding g =
      Pattern.parser >>= fun p ->
      symbol "=" >>= fun () ->
      parse g >>= fun e ->
      P.return (p, e)

    let parse_let g =
      symbol "let" >>= fun () ->
      parse_let_binding g >>= fun lb ->
      symbol "in" >>= fun () ->
      parse g >>= fun body ->
      P.return (Eval.Expression.let' [lb] body)

    let parse_token _g =
      P.next >>= fun t ->
      return (Eval.Expression.token t)

    let parse_group g =
      symbol "(" >>= fun () ->
      parse g >>= fun e ->
      symbol ")" >>= fun () ->
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

        P.null (`Symbol "(") parse_group;
        P.delimiter (`Symbol ")");
      ]

    let parser =
      parse grammar
  end

  and Pattern : sig
    val parser : Eval.Pattern.t P.parser
  end = struct
    let parse g =
      let left =
        P.current >>= fun token ->
        match token with
        | `Symbol name when Char.Ascii.is_upper (String.get name 0) ->
          P.some (P.nud 0 g) >>= fun (x, xs) ->
          if List.length xs = 0 then
            P.return x
          else
            P.return (Eval.Pattern.constructor (Eval.Name.id name) xs)
        | other -> P.nud 0 g
      in
        left >>= P.led 0 g

    let parse_token _g =
      P.current >>= fun t ->
      P.advance >>= fun () ->
      P.return (Eval.Pattern.token t)

    let parse_cons g e1 =
      P.consume (`Symbol "&") >>= fun () ->
      parse g >>= fun e2 ->
      P.return (Eval.(Pattern.constructor (Name.id "&")) [e1; e2])

    let grammar = P.grammar [
        P.term parse_token;
        P.left 50 (`Symbol "&") parse_cons;
        P.between (`Symbol "(") (`Symbol ")") identity;
        P.delimiter (`Symbol ")");
        P.delimiter (`Symbol "=");
      ]
    let parser = parse grammar
  end

  module Statement = struct
    let parse_val g =
      P.consume (`Symbol "val") >>= fun () ->
      Pattern.parser >>= fun p ->
      P.consume (`Symbol "=") >>= fun () ->
      Expression.parser >>= fun e ->
      return (Eval.Statement.val' p e)

    let grammar = P.grammar [
        P.null (`Symbol "val") parse_val;
      ]

    let parser =
      P.parse grammar
  end

  module Module = struct
    let parser : Eval.Module.t P.parser =
      (* FIXME: Pratt.many is not good for error reporting! *)
      P.many Statement.parser
  end
end

