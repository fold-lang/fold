open Prelude

val expression : ?loc:loc -> fl -> Ml.expression
val structure : fl -> Ml.structure

module Embed : sig
  val encode : fl -> Ml.expression
  val decode : Ml.expression -> fl
end
