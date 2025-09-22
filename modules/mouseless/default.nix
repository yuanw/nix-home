{ config, lib, ... }:

with lib;
let
  cfg = config.modules.mouseless;
in
{

  options.modules.mouseless = {
    enable = mkEnableOption "mouseless";
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.modules.brew.enable;
        message = "need homebrew to install mouseless (for now)";
      }

    ];
    brew = {
      casks = [
        "mouseless@preview"

      ];
    };

  };
}
