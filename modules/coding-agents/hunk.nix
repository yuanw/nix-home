{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.hunk;
in
{
  options.modules.hunk = {
    enable = lib.mkEnableOption "hunk";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.hunk;
      description = "Which hunk package to use.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ cfg.package ];
    };
  };
}
