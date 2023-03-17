# JOURNAL

## 2023-03-17

Consider always parsing scopes with the base grammar.

### Handling base shaper tokens

When a nested grammar encounters base tokens like `(`, `)`, `,` or `;`, how should they be parsed?

Questions:
- Should the nested grammar be able to override core tokens?
- Should the nested grammar delegate parsing to the core grammar for nested tokens?
  - In which case, how should grammar results be lifted from core to nested?


Options when a base token is encountered:
1. Nested grammar stops (with a recoverable error) and allows the core grammar to take over.
  - Prevents nested grammars from overriding base tokens!
2. Nested grammar includes rules for handling core tokens.
  - Prevents nested grammars from overriding base tokens!
3. The parsing of the base tokens is a builtin feature of nested grammars.
  - The grammar requires handlers for processing results parsed from base tokens.

Notes:
- The reality is that shaper's base grammar is not useful on its own. Evaluating to the primitive Shaper AST might be useful for cases where a materialized token tree is needed, but isn't generally preferrable since the streaming linear approach is more optimal.
- A core requirement for handling grammar composition is robustness of scope. A central promise of shaper is the ability to precisely and consistently delimit language scopes.


### Abstract syntax type

Partially inspired by: "Finally, Safely-Extensible and EfficientLanguage-Integrated Query".

```ocaml
module type Symantics = sig
  type 'a t

  val lower : string -> 'a t
  val upper : string -> 'a t

  val int : int -> int t
  (* ... *)

  val seq : 'a t list -> 'a list t
  val parens : 'a t -> 'a t
  val braces : 'a t -> 'a t
end
```

This tagless-final encoding would allow any target language to directly construct it's evaluation result without going through the intermediate shaper AST.

The type variable ['a] can be used to attach additional semantic interpretation to the encoded values. For example, if `braces items` encodes a record value in some target language, the resulting type could be `record t`.

Of course, the limited surface of encoders prevents us from having "high-resolution" type-level annotations. The individual AST constructs could still be reperesented as internal runtime tags in `'a`.


### Overriding base tokens like `;`

If the nested grammar decides to override a base token like `;` can this break composability?

Suppose `;` is used for debugging expressions: `(2+2);1` prints `4` and evaluates to `1`.

This should limit the scope of `calc` until `;`:
```
calc! 2; 1
```

This includes `;` in the calc scope:
```
calc! (2; 1)
```

The problem with this is that the parsing rules are currently greedy so both versions parse to consume the full scope as part of `calc`.


### Running base grammar in "lockstep" with nested grammar

This technique would ensure that the base grammar takes priority over nested grammars in all cases. The base grammar would implement the "skip" interpreter. It's only practical role is to ensure scope delimitation.

Considering that, as we saw in the previous section, nested grammar can override base tokens and the standard implementation of the pratt parser is greedy, this "lockstep" approach is very desirable.

An alternative to that would be to manually inject token handlers into the nested grammar.

In combination with antiquotation sytnax this is further reinforced.

#### Questions

- Do we have an inverted control problem?
- Nested grammars are pull-based. Do we need them to be push-based to allow the parent grammar to have easier control over filtering? Think "sinks".
- When running a nested parser do we need to check if it's going to be run in scoped mode?
  - Lexer.pick to see if it's an L scope token.
  - If it is scoped we don't need to override seq delimiters (`,` and `;`).
  - If it is unscoped we need to make sure the nested parser terminates at a seq delim.


#### How does it work?

- The base grammar wraps the nested grammar.
- The base grammar processes tokens and either:
  - forwards them to the nested grammar; or
  - decides to terminate the processing.
- The nested grammar result is injected (transformed) into the base grammar result (either AST or whatever the host language is?)

> Note: this is somewhat similar to "reducer" wrapping. Specially considering that nested grammars might produce arbitrary types.


## 2023-03-16

How can the base shaper grammar rules be precisely overimposed over embedded grammars?

For example:

```
a, (calc! (2 + 1)), b
```

The last `)` must be processed by the base grammar and not the inner calc grammar.
