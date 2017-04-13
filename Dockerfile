FROM ocaml/opam:alpine

RUN sudo apk add ncurses

RUN opam update

RUN opam depext conf-m4

RUN opam pin add -y -k git pure https://github.com/rizo/pure
RUN opam pin add -y -k git iter https://github.com/rizo/iter
RUN opam pin add -y -k git ppx_format https://github.com/rizo/ppx_format

RUN opam install -y sedlex ppx_deriving fmt

VOLUME /fold
WORKDIR /fold

CMD bash

