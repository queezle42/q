{ pkgs, qd, args ? {} }:

let
  lib = pkgs.lib;
  haskell = pkgs.haskell;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      qd = qd;
    };
  };

  rawdrv = haskellPackages.callCabal2nix "q" ./. args;
  drv =  haskell.lib.generateOptparseApplicativeCompletions [ "q" ] rawdrv;

in
  if lib.inNixShell then drv.env else drv
