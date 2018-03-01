open Local
open Lex

module Make (Eval : Interpreter.Self) = struct
  type expression = Eval.Expression.t
  type pattern = Eval.Pattern.t

  module P = Pratt.Make(Token)
  let (>>=)  = P.(>>=)
  let return = P.return

  let run' p input = P.run p input

  let run p input =
    match P.run p input with
    | Ok (x, input') when Pratt.Stream.is_empty input' -> Ok x
    | Ok (x, input') -> raise (Failure "XXX: Input letfover")
    | Error e -> Error e

  let symbol s = P.consume (`Symbol s)

  let sep1 ~by:sep p =
    p >>= fun x ->
    P.many (sep >>= fun () -> p) >>= fun xs ->
    P.return (x, xs)

  module rec Expression : sig
    val parser : expression P.parser
  end = struct
    let parse ?precedence:(rbp = 0) g =
      let left =
        P.some (P.nud rbp g) >>= fun (x, xs) ->
        if List.length xs = 0 then
          P.return x
        else
          P.return (Eval.Expression.apply x xs) in
      left >>= P.led rbp g

    let parse_let_binding g =
      Pattern.parser >>= fun p ->
      symbol "=" >>= fun () ->
      parse g >>= fun e ->
      P.return (p, e)

    let parse_let g =
      symbol "let" >>= fun () ->
      sep1 ~by:(symbol ",") (parse_let_binding g) >>= fun (lb, lbs) ->
      symbol "in" >>= fun () ->
      parse g >>= fun body ->
      P.return (Eval.Expression.let' (lb :: lbs) body)

    let parse_token _g =
      P.next >>= fun t ->
      return (Eval.Expression.token t)

    let parse_group_or_tuple g =
      symbol "(" >>= fun () ->
      sep1 ~by:(symbol ",") (parse g) >>= fun (x, xs) ->
      symbol ")" >>= fun () ->
      if List.length xs = 0 then
        return x
      else
        return (Eval.Expression.tuple (x :: xs))

    let parse_token _g =
      P.current >>= fun t ->
      P.advance >>= fun () ->
      P.return (Eval.Expression.token t)

    let infix_left precedence tok f =
      let p g a =
        P.advance >>= fun () ->
        parse ~precedence g >>= fun b ->
        P.return (f a b) in
      P.left precedence tok p

    let grammar = P.grammar [
        P.term parse_token;

        P.null (`Symbol "let") parse_let;

        infix_left 30 (`Symbol "+")
          (fun a b -> Eval.Expression.(apply (token (`Symbol "+")) [a; b]));

        (* Isn't strictly part of this grammar but needed to stop parsing. *)
        (* Should not be needed *)
        P.delimiter (`Symbol "val");
        P.delimiter (`Symbol "def");
        P.delimiter (`Symbol "in");
        P.delimiter (`Symbol ",");

        P.null (`Symbol "(") parse_group_or_tuple;
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
      P.many Statement.parser >>= fun xs ->
      return (Eval.Module.make xs)
  end
end

