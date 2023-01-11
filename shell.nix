{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs86", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, mtl, parsec, stdenv, text, chili, hsx2hs, ghcjs-base
      }:
      mkDerivation {
        pname = "stackulator";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base containers mtl parsec text ];
        executableHaskellDepends = [ nixpkgs.haskellPackages.cabal-install base containers mtl parsec text chili hsx2hs ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
