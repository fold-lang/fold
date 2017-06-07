
open Pure
open Base

type error =
  | Empty

module Parser : sig
  type ('a, +'s) parser = ('s -> ('a * 's, error) result)

  (* Monad *)

  val pure : 'a -> ('a, 's) parser

  val (>>=) : ('a, 's) parser -> ('a -> ('b, 's) parser) -> ('b, 's) parser
end = struct
  type ('a, 's) parser = ('s -> ('a * 's, error) result)

  (* Monad *)

  let pure x =
    fun s -> Ok (x, s)

  let (>>=) ( p) f =
    (fun s ->
       match p s with
       | Ok (x, s') -> let  p' = f x in p' s'
       | Error e -> Error e)
end

open Parser


let error e =
  (fun _ -> Error e)

let (>>) p1 p2' =
  p1 >>= fun _ -> p2' ()

(* State *)
let run ( p) s = p s

let put s =
  (fun _ -> Ok ((), s))

let zero =
  (fun s -> Ok ((), s))


(* Alternative *)

let empty =  (fun _ -> Error Empty)

let (<|>) p1 p2 =
  (fun s ->
     match p1 s with
     | Ok x -> Ok x
     | Error _ -> p2 s)

