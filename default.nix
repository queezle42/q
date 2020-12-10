{ nixpkgs ? import <nixpkgs> {}, args ? {} }:

let
  inherit (nixpkgs) lib haskellPackages haskell;

  rawdrv = haskellPackages.callCabal2nix "q" ./. args;
  drv =  haskell.lib.generateOptparseApplicativeCompletions [ "q" ] rawdrv;

in
  if lib.inNixShell then drv.env else drv
