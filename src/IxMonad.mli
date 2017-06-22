
type ('a, 'i, 'j) t = 'i -> 'a * 'j

val pure : 'a -> ('a, 'i, 'i) t
val ( >>= ) : ('a, 'i, 'j) t -> ('a -> ('b, 'j, 'k) t) -> ('b, 'i, 'k) t

val get : ('i, 'i, 'i) t
val put : 'j -> (unit, 'i, 'j) t

