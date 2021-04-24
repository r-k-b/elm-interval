{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "elm-interval";

  buildInputs = with pkgs; [
    elm2nix
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-review
    elmPackages.elm-test
    nixfmt
  ];

  shellHook = "";
}

