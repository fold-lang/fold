open Local
open Lex

module Make (Eval : Interpreter.Self) = struct

  module P = Pratt.Make(Token)
  let (>>=)  = P.(>>=)
  let return = P.return

  let run = P.run

  module Expression = struct

    let grammar = P.grammar [
        P.term (fun g -> P.next >>= (return << Eval.Expression.token));

        P.left 30 (`Symbol "+")
          (P.binary (fun a b -> Eval.Expression.(apply (token (`Symbol "+")) [a; b])));

        (* Isn't strictly part of this grammar but needed to stop parsing. *)
        (* Should not be needed *)
        P.delimiter (`Symbol "val");

        (* Recursive call to parse uses the current `g` potentially updated in RT. *)
        P.null (`Symbol "(")
          (fun g -> P.advance >>= fun () ->
            P.parse g >>= fun x ->
            P.consume (`Symbol ")") >>= fun () ->
            return x);

        P.delimiter (`Symbol ")");
      ]

    let parser =
      P.some (P.parse grammar) >>= fun (x, xs) ->
      if List.length xs == 0 then
        return x
      else
        return (Eval.Expression.apply x xs)
  end

  module Pattern = struct
    let constructor g =
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
        P.term constructor;
        P.between (`Symbol "(") (`Symbol ")") identity;
        P.delimiter (`Symbol ")");
        P.delimiter (`Symbol "=");
      ]
    let parser = P.parse grammar
  end

  module Statement = struct
    let grammar = P.grammar [
        P.null (`Symbol "val") begin fun g ->
          P.consume (`Symbol "val") >>= fun () ->
          Pattern.parser >>= fun p ->
          P.consume (`Symbol "=") >>= fun () ->
          Expression.parser >>= fun e ->
          return (Eval.Statement.val' p e)
        end;

        P.null (`Symbol "def") begin fun g ->
          P.consume (`Symbol "def") >>= fun () ->
          Pattern.parser >>= fun p ->
          P.consume (`Symbol "=") >>= fun () ->
          Expression.parser >>= fun e ->
          return (Eval.Statement.val' p e)
        end;
      ]
    let parser = P.parse grammar
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
