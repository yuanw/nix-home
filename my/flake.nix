{
  description = "user information";
  inputs = { };
  outputs = { self, nixpkgs }: {

    my = import ./my.nix;

  };
}
