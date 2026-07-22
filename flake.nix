{
  description = "Graded Monads with a Qualified Do Interface";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-26.05;
    flake-utils.url = github:numtide/flake-utils;
    monoidal-functors = {
      url = github:solomon-b/monoidal-functors/e31eca0bb9165a7711a9c2632a953f42ea42c067;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, monoidal-functors }:
    let
      ghcVersion = "9103";
      compiler = "ghc${ghcVersion}";
      overlay = import ./overlay.nix;
      overlays = [ monoidal-functors.overlays.default overlay ];
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
        in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal-install
              haskell.compiler.${compiler}
              haskell.packages.${compiler}.haskell-language-server
              just
              nixpkgs-fmt
              ormolu
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree
            {
              graded-monads = pkgs.haskellPackages.graded-monads;
            } // {
            default = pkgs.haskellPackages.graded-monads;
          };
        }) // {
      overlays.default = overlay;
    };
}
