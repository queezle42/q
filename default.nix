{
  pkgs,
  haskellPackages ? pkgs.haskellPackages,
  enableStatic ? pkgs.stdenv.targetPlatform.libc == "musl",
  args ? {}
}:

let
  lib = pkgs.lib;
  haskell = pkgs.haskell;

  #rawDrv = pkgs.haskell.packages.ghc921.callCabal2nix "q" ./. args;
  rawDrv = haskellPackages.callCabal2nix "q" ./. args;

  staticDrv = haskell.lib.overrideCabal rawDrv (old: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
      "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
      #"--disable-executable-stripping"
    ];
  });

  drv = haskell.lib.generateOptparseApplicativeCompletions [ "q" ]
    (if enableStatic then staticDrv else rawDrv);

in
  if lib.inNixShell then drv.env else drv
