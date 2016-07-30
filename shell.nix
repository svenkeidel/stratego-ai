{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  texEnv = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-small pgf tikz-cd cm-super stmaryrd;
  };

  f = { mkDerivation, base, mtl, stdenv, text, transformers, Cabal, cabal-install }:
      mkDerivation {
        pname = "system-s";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base mtl text transformers Cabal cabal-install ];
        description = "Abstract Interpreter for Stratego";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
