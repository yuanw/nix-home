self: super: {

  ormolu =
    let source = super.fetchFromGitHub {
      owner = "tweag";
      repo = "ormolu";
      rev = "de279d80122b287374d4ed87c7b630db1f157642"; # update as necessary
      sha256 = "0qrxfk62ww6b60ha9sqcgl4nb2n5fhf66a65wszjngwkybwlzmrv"; # as well
    }; in
      (import source { pkgs = self; }).ormolu;
}
