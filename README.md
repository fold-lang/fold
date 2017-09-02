## The _Fold_ Programming Language

_Fold_ is an expressive, pragmatic functional language designed for development of fast and robust applications.

<table>
  <tbody>
    <tr>
      <td><strong>Paradigms</strong></td>
      <td>functional, compiled, imperative, generic</td>
    </tr>
    <tr>
      <td><strong>Type discipline</strong></td>
      <td>static, strong, inferred, structural, nominal</td>
    </tr>
        <tr>
      <td><strong>Influenced by</strong></td>
      <td>OCaml, D, Lisp, Python</td>
    </tr>
        <tr>
      <td><strong>Platform</strong></td>
      <td>X86-64, ARM 32-64, JavaScript</td>
    </tr>
        <tr>
      <td><strong>Operating System</strong></td>
      <td>Linux, macOS, Windows, FreeBSD, Android, iOS</td>
    </tr>
        <tr>
      <td><strong>License</strong></td>
      <td>MIT</td>
    </tr>
  </tbody>
</table>


![fold-repl](https://github.com/rizo/fold-pratt/raw/master/resources/screenshot-1.png)

Currently the language parser is being implemented as a separate parsing library:

- [fold-lang/pratt-parser](https://github.com/fold-lang/pratt-parser)


### Documentation

- [Language Overview](https://github.com/fold-lang/fold/wiki/Language-Overview) – quick overview of the language for the impatient.
- [Language Guide](https://github.com/fold-lang/fold/wiki/Language-Guide) – introduces the language and the ecosystem.
- [Language Reference](https://github.com/fold-lang/fold/wiki/Language-Reference) – defines the formal specification of the language.


### Building

The project can be built with docker. The official image `foldlang/fold` can be used to build and test the compiler.

```bash
# Clone the repository
git clone https://github.com/fold-lang/fold
cd fold

# Start the docker container (the image will be fetched from the registry)
make docker-run

# Now inside the container we can setup and built the project
oasis setup
make build

# Fold compiler will be locally called as "Main.native"
./test_parser.byte
./Main.native
```

If you wish to build the image locally a [`Dockerfile`](https://github.com/fold-lang/fold/blob/master/Dockerfile) is provided. All
changes to the dependencies should be added to the `Dockerfile` to ensure that
the image can be always rebuild.


### Dependencies

- [**Sedlex**](https://github.com/alainfrisch/sedlex) – Unicode-friendly lexer generator.
- [**Pure**](https://github.com/rizo/pure) – Small and modular base library.
- [**Iter**](https://github.com/rizo/iter) – Efficient functorized iterators.
- [**Fmt**](https://github.com/dbuenzli/fmt) – Format pretty-printer combinators.
- [**ppx_format**](https://github.com/rizo/ppx_format) – Simple Python-inspired syntax for string formatters.
- [**ppx_deriving**](https://github.com/whitequark/ppx_deriving) – Type-driven code generation.
- [**Cmdliner**](https://github.com/dbuenzli/cmdliner) – Declarative definition of command line interfaces.


### Contributing

Any and all contributions are very much welcomed. Please fork the repository and send a pull request to the master branch.

Thank you for your interest and have a nice day.

