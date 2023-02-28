type t =
  | Eps (* ε         *)
  | Sym of string (* "x"       *)
  | Any of string (* a         *)
  | Seq of t list (* a b ...   *)
  | Alt of t list (* a | b ... *)
  | Opt of t (* a?        *)
  | Many of t (* a*        *)
  | Some of t (* a+        *)

let ( => ) a b = (a, b)

let rules =
  [ (* let *)
    `Seq [ `Sym "let"; `Opt (`Sym "rec"); `Any ] => ()
  ; (* if *)
    `Seq
      [ `Sym "if"
      ; `Sym "("
      ; `Any
      ; `Sym ")"
      ; `Sym "{"
      ; `Any
      ; `Sym "}"
      ; `Opt (`Seq [ `Sym "else"; `Sym "{"; `Any; `Sym "}" ])
      ]
    => ()
  ; (* while *)
    `Seq [ `Sym "while"; `Sym "("; `Any; `Sym ")"; `Sym "{"; `Any; `Sym "}" ]
    => ()
  ; (* { ... } *)
    `Seq [ `Sym "{"; `Any; `Sym "}" ] => ()
  ; (* a = b *)
    `Infix "=" => ()
  ; (* fn a -> b *)
    `Seq [ `Sym "fn"; `Some `Any; `Sym "->"; `Any ] => ()
  ]
