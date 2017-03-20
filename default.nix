{ pkgs ? import <nixpkgs> {} }:

let
  hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install attoparsec hlint text containers unordered-containers hspec mtl
    pretty arrows
  ]);

  texEnv = (pkgs.texlive.combine {
      inherit (pkgs.texlive) scheme-small pgf tikz-cd cm-super stmaryrd stix syntax latexmk;
  });

in pkgs.stdenv.mkDerivation {
  name = "system-s";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv # texEnv
  ];
}
