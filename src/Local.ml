open Pure

let const2 x _ _ = x

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

let (++) = List.append


let print ?(out = Pervasives.stdout) ?(endline = "\n") ?(flush = true) str =
  Pervasives.output_string out (str ^ endline);
  if flush then Pervasives.flush out else ()


let (or) self lazy_default =
  match self with
  | Some x -> x
  | None -> Lazy.force lazy_default


module List = struct
  include List
  let head = function [] -> None | x::_ -> Some x
end


module Option = struct
  include Option

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
      Option.catch (fun () -> find k m)
  end
end


module Fmt = struct
  include Fmt

  let pl fmt =
    kpf (fun formatter -> Format.pp_print_newline formatter ()) stdout fmt
end

