# nix-uci glue: evalModules -> JSON -> a script that emits `uci batch` commands.
# Vendored shape from Mic92's openwrt/nix/default.nix, minus the sops coupling
# (secrets now arrive through $NIX_UCI_SECRETS at run time).
{ pkgs }:
let
  nix-uci = pkgs.python3.pkgs.buildPythonApplication {
    pname = "nix-uci";
    version = "0.1";
    pyproject = true;
    src = ./.;
    build-system = [ pkgs.python3Packages.setuptools ];
    meta.mainProgram = "nix-uci";
  };
in
{
  writeUci =
    configuration:
    let
      res = pkgs.lib.evalModules {
        modules = [
          { _module.args = { inherit pkgs; }; }
          ./module-options.nix
          configuration
        ];
      };
      json = (pkgs.formats.json { }).generate "uci.json" {
        inherit (res.config.uci) settings raw;
      };
    in
    {
      inherit json;
      command = pkgs.writeShellScript "uci-commands" ''
        exec ${nix-uci}/bin/nix-uci "${json}"
      '';
    };
}
