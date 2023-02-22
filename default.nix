{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;

  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "734386c62f4022d0d48ebd7d20b2d41915bbd1dd";
  }) {
    inherit pkgs ocamlPackages;
    verbosity = "debug";
  };

in onix.env {
  repo = { url = "https://github.com/ocaml/opam-repository.git"; };
  path = ./.;
  gitignore = ./.gitignore;
  deps = { "ocaml-system" = "*"; };
  vars = {
    "with-dev-setup" = true;
    "with-test" = true;
    "with-doc" = true;
  };
}
