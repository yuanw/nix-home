{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.open-code-review;
in
{
  options.modules.open-code-review = {
    enable = lib.mkEnableOption "open-code-review (ocr)";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.open-code-review;
      description = "Which open-code-review package to use; provides the `ocr` command.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ cfg.package ];
    };
  };
}
