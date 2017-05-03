
(*
open Pure
module Parser = Pratt.Parser
open Lex
open Syntax

let (>>=) = Parser.(>>=)
let (>>) = Parser.(>>)


let define_delimiter name g =
  g
  |> Pratt.Grammar.define_prefix name Pratt.invalid_prefix
  |> Pratt.Grammar.define_infix name (Pratt.invalid_infix, 0)



let atom token =
  Parser.consume token >>= fun () ->
  Parser.pure (Expr.atom token)


let binary_infix token precedence =
  let parse left =
    Parser.consume token >>= fun () ->
    Pratt.prefix precedence >>= fun right ->
    Parser.pure (Form [Atom token; left; right]) in
  (parse, precedence)


let lookup_operator token =
  token
  |> Precedence.lookup
  |> Option.map (fun p -> (binary_infix token p, p))


module State = struct
  module Grammar = struct

    type rule =
      | Infix  of infix
      | Prefix of prefix

    type t = {
      prefix : prefix M.t;
      infix  : infix  M.t;
      next   : t option;
    }

    let empty = {
      prefix = M.empty;
      infix  = M.empty;
      next   = None;
    }


    let define_infix name rule self =
      { self with infix = M.add name rule self.infix  }


    let define_prefix name rule self =
      { self with prefix = M.add name rule self.prefix }


    let define name rule =
      match rule with
      | Infix  infix  -> define_infix  name infix
      | Prefix prefix -> define_prefix name prefix


    let rec lookup_prefix name { prefix; next } =
      match M.find name prefix with
      | None -> Option.(next >>= lookup_prefix name)
      | some -> some


    let rec lookup_infix name { infix; next } =
      match M.find name infix with
      | None -> Option.(next >>= lookup_infix name)
      | some -> some



    let infix token self =
      undefined ()

    let prefix token self =
      undefined ()
  end
end
   *)
