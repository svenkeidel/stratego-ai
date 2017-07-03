{ pkgs ? import <nixpkgs> {} }:

let hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install attoparsec hlint text containers unordered-containers hspec mtl
    pretty arrows fgl criterion stylish-haskell
  ]);

  texEnv = (pkgs.texlive.combine {
      inherit (pkgs.texlive) scheme-small pgf tikz-cd cm-super stmaryrd stix syntax latexmk;
  });

  # newggplot2 = pkgs.rPackages.ggplot2.overrideDerivation (oldAttrs: {
  #   src = pkgs.fetchFromGitHub {
  #     owner = "tidyverse";
  #     repo = "ggplot2";
  #     rev = "f4398b6d07c85c2d5cd0afb52f50948cba5b8674";
  #     sha256 = "1qr5dkcf9zmqx7achxgqsx80vqfic69zip6f6ppc5xs9aqcd9762";
  #   };
  # });
  
  rEnv = pkgs.rWrapper.override {
    packages = with pkgs.rPackages; [
      devtools dplyr tikzDevice ggplot2
    ];
  };

in pkgs.stdenv.mkDerivation {
  name = "system-s";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv rEnv texEnv
  ];
}
