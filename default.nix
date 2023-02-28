{ pkgs ? import <nixpkgs> { } }:

let
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;

  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "f28db771d96eb923d2a525e6c42d92d710764647";
  }) {
    inherit pkgs ocamlPackages;
    verbosity = "debug";
  };

in onix.env {
  repo = { url = "https://github.com/ocaml/opam-repository.git"; };
  path = ./.;
  env-file = ./.onix.env;
  gitignore = ./.gitignore;
  deps = { "ocaml-system" = "*"; };
  vars = {
    "with-dev-setup" = true;
    "with-test" = true;
    "with-doc" = true;
  };
}
