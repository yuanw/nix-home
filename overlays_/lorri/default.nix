self: super: {

  lorri =
    let
      master = builtins.fromJSON( builtins.readFile ./lorri.json);
      src = super.fetchFromGitHub {
          owner  = "target";
          repo   = "lorri";
          inherit (master) rev sha256;
          fetchSubmodules = true;
        }; in
      import src { inherit src; pkgs = self; };
}
