{
  description = "Intervals for Elm";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let supportedSystems = with flake-utils.lib.system; [ x86_64-linux ];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ];
        };
        inherit (pkgs) lib stdenv callPackage;
        inherit (lib) fileset hasInfix hasSuffix;
      in {
        packages = { };
        checks = { };
        devShells.default = pkgs.mkShell {
          name = "elm-interval-devenv";

          buildInputs = with pkgs; [
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-json
            elmPackages.elm-review
            elmPackages.elm-test
            just # for discoverable project-specific commands. Simpler than Make, plus Nix already handles the build system.
            nixfmt-classic
            nodejs
          ];

          shellHook = ''
            echo ""
            echo "This is the dev shell for the elm-interval project."
            just --list --list-heading $'Run \'just\' to see the available commands:\n'
            echo ""
          '';
        };
      });
}
