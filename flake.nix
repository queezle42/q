{
  description = "Hello World!";

  inputs = {
    quasar = {
      url = gitlab:jens/quasar?host=git.c3pb.de;
      inputs.nixpkgs.follows = "nixpkgs";
      follows = "quasar-network/quasar";
    };
    quasar-network = {
      url = gitlab:jens/quasar-network?host=git.c3pb.de;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, quasar, quasar-network }:
  let
    lib = nixpkgs.lib;
    systems = lib.platforms.unix;
    forAllSystems = f: lib.genAttrs systems (system: f system);
  in {
    packages = forAllSystems (system: {
      q = import ./. {
        pkgs = import nixpkgs { inherit system; overlays = [ quasar.overlay quasar-network.overlay ]; };
      };
    });

    overlay = self: super:  { q = import ./. { pkgs = self; }; };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.q;
    devShell.x86_64-linux = self.defaultPackage.x86_64-linux.env;
  };
}
