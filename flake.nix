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
        qd = qd.packages.x86_64-linux.qd;
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      };
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.q;
    devShell.x86_64-linux = self.defaultPackage.x86_64-linux.env;
  };
}
