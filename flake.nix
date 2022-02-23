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
    packages = forAllSystems (system:
      let
        nixpkgs' = (import nixpkgs { inherit system; overlays = [ self.overlay quasar-network.overlays.quasar quasar-network.overlay ]; });
      in {
        q = nixpkgs'.pkgs.haskellPackages.q;

        pkgsCross.aarch64-multiplatform.q = nixpkgs'.pkgsCross.aarch64-multiplatform.haskellPackages.q;

        pkgsMusl.q = nixpkgs'.pkgsMusl.haskellPackages.q;
      }
    );

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

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.q;
    devShell.x86_64-linux = self.defaultPackage.x86_64-linux.env;
  };
}
