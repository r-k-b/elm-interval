{
  description = "elm-interval";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) callPackage lib stdenv;
      in {
        defaultPackage = stdenv.mkDerivation {
          name = "make_elm-interval";
          src = ./.;
          buildInputs = with pkgs; [
            elmPackages.elm
            elmPackages.elm-format
            elmPackages.elm-review
            elmPackages.elm-test
          ];
          buildPhase = ''
            export ELM_HOME=./elm_home
            export PATH=./bin:$PATH
            patchShebangs ./bin
            # looks like we need https://github.com/cachix/elm2nix/issues/28
            # before we can do a pure build?
            # Currently it fails at the elm-test step, when attempting to
            # download dependencies.
            make
          '';
        };
        devShell = import ./shell.nix { inherit pkgs; };
      });
}
