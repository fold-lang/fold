{ pkgs ? import <nixpkgs> { } }:

let
  onix = import (builtins.fetchGit {
    url = "https://github.com/odis-labs/onix.git";
    rev = "969b4582eb7248330cf07d602723a26d7b4f4b71";
  }) {
    inherit pkgs;
    verbosity = "debug";
  };

in onix.env {
  path = ./.;
  gitignore = ./.gitignore;
  deps = { "ocaml-system" = "5.2.0"; };
  vars = {
    "with-dev-setup" = true;
    "with-test" = true;
    "with-doc" = true;
  };
}
