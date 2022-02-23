{
  description = "Hello World!";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    quasar-network = {
      url = gitlab:jens/quasar-network?host=git.c3pb.de;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, quasar-network }:
  let
    lib = nixpkgs.lib;
    systems = lib.platforms.unix;
    forAllSystems = lib.genAttrs systems;
  in {
    packages = forAllSystems (system: {
      q = import ./. {
        pkgs = import nixpkgs { inherit system; overlays = [ quasar-network.overlays.quasar quasar-network.overlay ]; };
      };
      aarch64-multiplatform.q =
        (import nixpkgs { inherit system; overlays = [ self.overlay quasar-network.overlays.quasar quasar-network.overlay ]; }).pkgsCross.aarch64-multiplatform.haskellPackages.q;

      static.q = (import nixpkgs { inherit system; overlays = [ self.overlays.static quasar-network.overlays.quasar quasar-network.overlay ]; }).pkgsMusl.haskellPackages.q;
    });

    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
          q = import ./. {
            pkgs = final;
            haskellPackages = hfinal;
          };
        };
      };
    };

    overlays.static = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: prev.haskell.packageOverrides hfinal hprev // {
          q = final.haskell.lib.overrideCabal
            (import ./. { pkgs = final; haskellPackages = hfinal; })
            (old: {
              enableSharedExecutables = false;
              enableSharedLibraries = false;
              configureFlags = [
                "--ghc-option=-optl=-static"
                "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${final.zlib.static}/lib"
                "--extra-lib-dirs=${final.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                "--extra-lib-dirs=${final.ncurses.override { enableStatic = true; }}/lib"
                "--disable-executable-stripping"
              ];
            });
        };
      };
    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.q;
    devShell.x86_64-linux = self.defaultPackage.x86_64-linux.env;
  };
}
