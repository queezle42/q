{
  description = "Hello World!";

  outputs = { self, nixpkgs }:
  {
    defaultPackage.x86_64-linux =
      import self {
        nixpkgs = import nixpkgs { system = "x86_64-linux"; };
      };
    devShell.x86_64-linux = self.defaultPackage.x86_64-linux.env;
  };
}
