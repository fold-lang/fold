## The _Fold_ Programming Language

_Fold_ is a modern pragmatic functional language with concise, expressive, programmable syntax for development of blazingly fast and robust applications.

![fold-repl](https://github.com/fold-lang/pratt/raw/master/resources/screenshot-1.png)

Currently the language parser is being implemented as a separate parsing library:

- [fold-lang/pratt-parser](https://github.com/fold-lang/pratt-parser)


### Documentation

- [Language Overview](https://github.com/fold-lang/fold/wiki/Language-Overview) – quick overview of the language for the impatient.
- [Language Guide](https://github.com/fold-lang/fold/wiki/Language-Guide) – introduces the language and the ecosystem.
- [Language Reference](https://github.com/fold-lang/fold/wiki/Language-Reference) – defines the formal specification of the language.


### Contributing

Any and all contributions are very much welcomed. Please fork the repository and send a pull request to the master branch.

Thank you for your interest and have a nice day.


### Building

The project can be built with docker. The official image `foldlang/fold` can be used to build and test the compiler.

```bash
# Clone the repository (change ~/fold to preferred path)
git clone https://github.com/fold-lang/fold ~/fold
cd ~/fold

# Start the docker container (the image will be fetched from the registry)
docker run -it -v $PWD:/fold foldlang/fold bash

# Now inside the container run "make".
make build

# Fold compiler will be locally called as "Main.native".
./test_parser.byte
./Main.native
```

If you wish to build the image locally a [`Dockerfile`]() is provided. All
changes to the dependencies should be added to the `Dockerfile` to ensure that
the image can be always rebuild.


### Dependencies

- [**Sedlex**](https://github.com/alainfrisch/sedlex) – Unicode-friendly lexer generator.
- [**Pure**](https://github.com/rizo/pure) – Small and modular base library.
- [**Iter**](https://github.com/rizo/pure) – Efficient functorized iterators.
- [**Fmt**](https://github.com/dbuenzli/fmt) – Format pretty-printer combinators.
- [**ppx_format**](https://github.com/rizo/ppx_format) – Simple Python-inspired syntax for string formatters.
- [**ppx_deriving**](https://github.com/whitequark/ppx_deriving) – Type-driven code generation.
- [**Cmdliner**](https://github.com/dbuenzli/cmdliner) – Declarative definition of command line interfaces.

