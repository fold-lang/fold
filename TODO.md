# TODO

- [ ] Represent `fn` as a form with `->`:
  - Requires arrow to be `t list * t`.
  - `fn! (_ -> _)` and `fn! {}`.
- [ ] Make form: `Form of string * t list`.
- [ ] Report the location of unclosed brackets.
- [ ] Add spacing context to tokens.
  - When parsing `<x></x>` we need to break up the tokens.
  - Use a token_group for "glued" tokens.
- [ ] `a |> f` is ok, but `f a |> g` is not: undeclared infix operators use
  fallback infix rule wich is not triggered on juxt parsing.  