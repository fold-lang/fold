# TODO

- [ ] Represent `fn` as a form with `->`:
  - Requires arrow to be `t list * t`.
  - `fn! (_ -> _)` and `fn! {}`.
- [ ] Make form: `Form of string * t list`.
- [ ] Report the location of unclosed brackets.
- [ ] Add spacing context to tokens.
  - When parsing `<x></x>` we need to break up the tokens.
