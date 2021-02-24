{
  description = "Hello World!";

  inputs = {
    qd = {
      url = gitlab:jens/qd?host=git.c3pb.de;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, qd }:
  {
    packages.x86_64-linux.q =
      import ./. {
        pkgs = nixpkgs.legacyPackages.x86_64-linux // {
          haskellPackages = nixpkgs.legacyPackages.x86_64-linux.haskellPackages.override {
            overrides = hself: hsuper: {
              qd = qd.packages.x86_64-linux.qd;
            };
          };
        };
      };

    overlay = self: super:  { q = import ./. { pkgs = self; }; };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.q;
    devShell.x86_64-linux = self.defaultPackage.x86_64-linux.env;
  };
}
