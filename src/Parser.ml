open Local
open Lex

module S = Syntax

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
  val parser : S.Expression.t P.parser
end = struct
  let parse ?precedence:(rbp = 0) (g : S.Expression.t P.grammar) : S.Expression.t P.parser =
    let left =
      P.some (P.nud rbp g) >>= fun (x, xs) ->
      if List.length xs = 0 then
        P.return x
      else
        P.return (S.Expression.application x xs) in
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
    P.return (S.Expression.let' (lb :: lbs) body)

  let parse_token _g =
    P.next >>= fun t ->
    return (S.Expression.token t)

  let parse_group_or_tuple g =
    symbol "(" >>= fun () ->
    sep1 ~by:(symbol ",") (parse g) >>= fun (x, xs) ->
    symbol ")" >>= fun () ->
    if List.length xs = 0 then
      return x
    else
      return (S.Expression.tuple (x :: xs))

  let parse_token _g =
    P.current >>= fun t ->
    P.advance >>= fun () ->
    P.return (S.Expression.token t)

  let infix_left precedence tok f =
    let p g a =
      P.advance >>= fun () ->
      parse ~precedence g >>= fun b ->
      P.return (f a b) in
    P.left precedence tok p

  let parse_fn g =
    P.consume (`Symbol "fn") >>= fun () ->
    Pattern.parser >>= fun arg ->
    P.consume (`Symbol "->") >>= fun () ->
    parse g >>= fun body ->
    P.return (S.Expression.fn arg body)

  let parse_if g =
    P.consume (`Symbol "if") >>= fun () ->
    parse g >>= fun condition ->
    P.consume (`Symbol "then") >>= fun () ->
    parse g >>= fun consequence ->
    P.consume (`Symbol "else") >>= fun () ->
    parse g >>= fun alternative ->
    P.return (S.Expression.if' condition consequence alternative)

  let grammar = P.grammar [
      P.term parse_token;

      P.null (`Symbol "let") parse_let;

      infix_left 30 (`Symbol "+")
        (fun a b -> S.Expression.(application (token (`Symbol "+")) [a; b]));

      P.null (`Symbol "fn") parse_fn;
      P.null (`Symbol "if") parse_if;

      P.delimiter (`Symbol "val");
      P.delimiter (`Symbol "def");
      P.delimiter (`Symbol "then");
      P.delimiter (`Symbol "else");
      P.delimiter (`Symbol "in");
      P.delimiter (`Symbol ",");

      P.null (`Symbol "(") parse_group_or_tuple;
      P.delimiter (`Symbol ")");
    ]

  let parser =
    parse grammar
end

and Pattern : sig
  val parser : S.Pattern.t P.parser
end = struct
  let parse ?precedence:(rbp = 0) g =
    let left =
      P.some (P.nud rbp g) >>= fun (x, xs) ->
      if List.length xs = 0 then
        P.return x
      else
        P.return (S.Pattern.application x xs) in
    left >>= P.led rbp g

  let parse_token _g =
    P.current >>= fun t ->
    P.advance >>= fun () ->
    P.return (S.Pattern.token t)

  let parse_group_or_tuple g =
    symbol "(" >>= fun () ->
    sep1 ~by:(symbol ",") (parse g) >>= fun (x, xs) ->
    symbol ")" >>= fun () ->
    if List.length xs = 0 then
      return x
    else
      return (S.Pattern.tuple (x :: xs))

  let parse_cons g e1 =
    P.consume (`Symbol "&") >>= fun () ->
    parse g >>= fun e2 ->
    P.return (S.(Pattern.application (Pattern.token (`Symbol "&"))) [e1; e2])

  let grammar = P.grammar [
      P.term parse_token;
      P.left 50 (`Symbol "&") parse_cons;
      P.null (`Symbol "(") parse_group_or_tuple;
      P.delimiter (`Symbol ")");
      P.delimiter (`Symbol "=");
      P.delimiter (`Symbol "->");
    ]
  let parser = parse grammar
end


module Statement = struct
  let parse_val g =
    P.consume (`Symbol "val") >>= fun () ->
    Pattern.parser >>= fun p ->
    P.consume (`Symbol "=") >>= fun () ->
    Expression.parser >>= fun e ->
    return (S.Statement.val' p e)

  let parse_def g =
    P.consume (`Symbol "def") >>= fun () ->
    Pattern.parser >>= fun p ->
    P.consume (`Symbol "=") >>= fun () ->
    Expression.parser >>= fun e ->
    return (S.Statement.def p e)

  let grammar = P.grammar [
      P.null (`Symbol "val") parse_val;
      P.null (`Symbol "def") parse_def;
    ]

  let parser =
    P.parse grammar
end

