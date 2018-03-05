
module String = Astring.String
module Char = Astring.Char

let const2 x _ _ = x
let (<<) f g = fun x -> f (g x)
let identity x = x
let (@) f x = f x

let format = Printf.sprintf

let print ?(output = Pervasives.stdout) ?(break = "\n") fmt =
  let open Format in
  let formatter = formatter_of_out_channel output in
  let pp_break formatter =
    pp_print_string formatter break;
    pp_print_flush formatter () in
  kfprintf pp_break formatter fmt

exception Undefined
let undefined () = raise Undefined

module type Monoid = sig
  type t

  val empty : t

  val append : t -> t -> t

  val (++) : t -> t -> t
end

module Monoid = struct
  module type Base = sig
    type t

    val empty : t

    val append : t -> t -> t
  end

  module Make(B : Base) : Monoid with type t := B.t = struct
    include B

    let (++) = append
  end
end

let (++) = String.append

let log fmt =
  Fmt.kpf (fun f -> Fmt.pf f "@.") Fmt.stderr fmt


let (or) self lazy_default =
  match self with
  | Some x -> x
  | None -> Lazy.force lazy_default


module List = struct
  include List
  let head = function [] -> None | x::_ -> Some x
end


module Option = struct
  include Options.Option

  let empty = None

  let (<|>) a b =
    match a with
    | Some x -> a
    | None -> Lazy.force b
end


module Map = struct
  module type OrderedType = Map.OrderedType

  module Make(K : Map.OrderedType) = struct
    include Map.Make(K)

    let find k m =
      try Some (find k m) with
      | _ -> None
  end
end


module Fmt = struct
  include Fmt

  let pl fmt =
    kpf (fun formatter -> pf formatter "@.") stdout fmt

  let pp pp =
    Format.fprintf Fmt.stderr "@[%a@.@]" pp
end

