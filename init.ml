#require "pure";;
#require "iter";;
#require "sedlex";;
#require "fmt.top";;
#require "ppx_deriving.std";;
#directory "_build/src";;
#load_rec "_build/src/fold.cmo";;
open Fold;;
print "==> Fold library loaded";;

module P = Fold.Parser;;
open Lex;;
