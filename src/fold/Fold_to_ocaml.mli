open Prelude

val expression : ?loc:loc -> fl -> Parsetree.expression
val structure : fl -> Parsetree.structure

module Embed : sig
  val encode : fl -> Parsetree.expression
  val decode : Parsetree.expression -> fl
end
