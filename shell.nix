{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "advent-of-code-shell";

  buildInputs = with pkgs; [
    ghc
    ormolu
  ];
}
