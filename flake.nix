{
  description = "Hello World!";

  inputs = {
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

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.q;
    devShell.x86_64-linux = self.defaultPackage.x86_64-linux.env;
  };
}
