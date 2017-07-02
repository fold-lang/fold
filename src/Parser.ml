open Pure
open Base
open Lex

let default_operator token f =
  let open Pratt in
  token
  |> Precedence.lookup
  |> Option.map (fun precedence ->
      let parse g x =
        advance >>= fun () ->
        nud precedence g >>= fun y ->
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

module Init (Eval : Interpreter.Self) = struct

  module Expression = struct
    open Pratt

    let form token =
      let append x y =
        match x with
        | `Apply (f, xs) -> `Apply (f, List.append xs [y])
        | atom -> `Apply (atom, [y]) in
      let apply x y = Eval.Expression.apply (Eval.Expression.token token) [x; y] in
      default_operator token apply or lazy (juxtaposition token append)

    let atom token =
      singleton (Eval.Expression.token token)

    let grammar =
      Grammar.init ~atom ~form "Expression" (keywords ())

    let parse = Pratt.parse grammar
  end


  module Pattern = struct
    open Pratt

    let form token =
      let append x y =
        match x with
        | `Apply (f, xs) -> `Apply (f, List.append xs [y])
        | atom -> `Apply (atom, [y]) in
      let apply x y = Eval.Pattern.apply (Eval.Pattern.token token) [x; y] in
      default_operator token apply or lazy (juxtaposition token append)

    let atom token = singleton (Eval.Pattern.token token)

    let grammar =
      let rules = [
        delimiter "=";
      ] ++ keywords () in
      Grammar.init ~atom ~form "Pattern" rules

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

