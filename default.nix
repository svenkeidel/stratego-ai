{ pkgs ? import <nixpkgs> {} }:

let
  env = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install hlint mtl text containers hspec
  ]);

in pkgs.stdenv.mkDerivation {
  name = "system-s";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    env
  ];
}
