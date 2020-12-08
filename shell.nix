{ pkgs ? import <nixpkgs> { } }:

let
  ghc = pkgs.haskell.packages.ghc8102.ghcWithPackages
    (haskellPackages: with pkgs.haskellPackages; [ split ]);
in pkgs.mkShell {
  name = "advent-of-code-shell";

  buildInputs = with pkgs; [ ormolu ] ++ [ ghc ];
}
