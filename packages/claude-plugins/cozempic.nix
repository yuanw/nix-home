# Source and version follow pkgs.cozempic (packages/cozempic/default.nix)
{
  pkgs,
  mkClaudePlugin,
  ...
}:
let
  pyPkg = pkgs.cozempic;
  rev = "v${pyPkg.version}";
  src = pyPkg.src;
in
mkClaudePlugin {
  pname = "cozempic";
  inherit (pyPkg) version;
  inherit rev src;
  pluginSubdir = "plugin";
  marketplace = {
    name = "Ruya-AI";
    inherit src;
    owner = "Ruya-AI";
    repo = "cozempic";
  };
  runtimeInputs = [
    pkgs.uv
    pyPkg
  ];
}
