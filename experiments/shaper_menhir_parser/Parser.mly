
%{
open Shaper


%}

%token <float> NUMBER
%token <string> ID
%token <string> KWD

%token SEMI EQ ARROW COLON DOT PIPE TILDE COMMA DOT_DOT QUEST
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET

%token EOF

%right SEMI
%left COLON
%left EQ
%left PIPE
%left DOT
%right ARROW

%nonassoc UNARY

%start <'a t> root

%%

root:
  | e = syntax EOF {e}

scalar:
  | i = NUMBER 
      { Atom (Num i) } 
  | v = ID 
      { Atom (Sym v) }

syntax:
  | LPAREN x = syntax RPAREN 
      { x }
  | syn_1 = syntax SEMI syn_2 = syntax 
      { Seq (syn_1, syn_2) }
  | syn_1 = syntax ARROW syn_2 = syntax 
      { Arrow (syn_1, syn_2) }
  | syn_1 = syntax COLON syn_2 = syntax 
      { Constraint (syn_1, syn_2) }
  | syn_1 = syntax PIPE syn_2 = syntax 
      { Or (syn_1, syn_2) }
  | syn_1 = syntax EQ syn_2 = syntax 
      { Binding (syn_1, syn_2) }
  | syn_1 = syntax DOT syn_2 = syntax 
      { Field (syn_1, syn_2) }
  | TILDE l = ID syn = syntax %prec UNARY
      { Labeled (l, false, syn) }
  | s = scalar
      { s }
