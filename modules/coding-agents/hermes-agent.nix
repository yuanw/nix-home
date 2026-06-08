{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.hermes-agent;
in
{
  options.modules.hermes-agent = {
    enable = lib.mkEnableOption "hermes-agent";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.hermes-agent;
      description = "Which hermes-agent package to use.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ cfg.package ];
    };
  };
}
