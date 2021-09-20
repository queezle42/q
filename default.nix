{ pkgs, haskellPackages ? pkgs.haskellPackages, args ? {} }:

let
  lib = pkgs.lib;
  haskell = pkgs.haskell;

  #rawdrv = pkgs.haskell.packages.ghc921.callCabal2nix "q" ./. args;
  rawdrv = haskellPackages.callCabal2nix "q" ./. args;
  drv =  haskell.lib.generateOptparseApplicativeCompletions [ "q" ] rawdrv;

in
  if lib.inNixShell then drv.env else drv
