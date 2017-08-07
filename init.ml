#require "pure";;
#require "iter";;
#require "sedlex";;
#require "fmt.top";;
#require "pratt";;
#require "ppx_deriving.std";;
#require "compiler-libs.bytecomp";;
#directory "_build/src";;
#load_rec "_build/src/fold.cmo";;
open Fold;;
Pure.print "==> Fold library loaded";;

