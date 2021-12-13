{ pkgs ? import <nixpkgs> {} }:
let
  #easy-hls-src = pkgs.fetchFromGitHub {
  #  owner  = "jkachmar";
  #  repo   = "easy-hls-nix";
  #  rev    = "7c123399ef8a67dc0e505d9cf7f2c7f64f1cd847";
  #  sha256 = "sha256-/crlHEVB148PGQLZCsHOR9L5qgvCAfRSocIoKgmMAhA=";
  #};
  #easy-hls = pkgs.callPackage easy-hls-src {
  #  ghcVersions = ["9.0.1"];
  #};

  editorTooling = [
    pkgs.stylish-haskell
    pkgs.hlint
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.ghcid
    #pkgs.haskell.packages.ghc901.haskell-language-server
    #easy-hls
  ];

  buildDeps =
    [ pkgs.haskell.compiler.ghc901
      pkgs.zlib.dev
    ];

  dynamicLibraries = [ pkgs.zlib ];
in
pkgs.mkShell {
  buildInputs = editorTooling ++ buildDeps;
  LD_LIBRARY_PATH = pkgs.lib.strings.makeLibraryPath dynamicLibraries;
}
