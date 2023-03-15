# Notes

## Grammar

```
syntax =
  | literal
  | id
  | syntax syntax+
  | kwd syntax*
  | syntax kwd syntax*
  | "(" syntax ")"
  | "{" syntax "}"
  | "[" syntax "]"
  | syntax ("," syntax)+
  | syntax (";" syntax)+
  | id "!" token*
```


Basic shaper grammar:
```
syntax =
  | const
  | ident
  | "(" syntax* ")"
  | "{" syntax* "}"
  | "[" syntax* "]"
  | syntax syntax+
  | syntax ("," syntax)* ","?
  | syntax (";" syntax)* ";"?
```

## AST

```
type delimiter = {
| Braces
| Brakets
| Parens
}

type separator = {
| Semicolon
| Comma
}

type ast = {
| Symbol string
| Literal literal
| Group delimiter ast
| Seq sep (list ast)
}
```

```
type node = {
| Ident string
| Const const
| Group group
}

and const = {
| Integer string (option char)
| Char char
| String string loc (options string)
| Float string (option char)
}

and group = {
  separator : option string,
  brackets : option (string, string),
  items : node list,
}
```

## Labels

Possible shapes:
```
~(a : t)
~(a? : t = 42)
~(a? : t)
~(a? = 1)
~a
~a:(b : t)
~a:b
~a?
~a?:(b : t)
~a?:(b = 1)
~a?:(b : t = 42)
~a?:b
```

Grammar:
```
((~)! ((:)! a t))
((~)! ((=)! ((:)! ((?)! a) t) 42))
((~)! ((:)! ((?)! a) t))
((~)! ((=)! ((?)! a) t))
((~)! a)

((~)! a)
~a:(b : t)

~a:b
~a?
~a?:(b : t)
~a?:(b = 1)
~a?:(b : t = 42)
~a?:b
```

Syntax:
```
syntax "label" = {
  | `~` (label : id) -> Syntax.Form ("~", [label])
  | `~` (label : id) -> Syntax.Form ("~", [label])
}
```
