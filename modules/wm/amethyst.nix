{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.wm.amethyst;
in
{

  options.modules.wm.amethyst = { enable = mkEnableOption "amethyst"; };
  config = mkIf cfg.enable {
 assertions = [
      {
        assertion = config.modules.brew.enable;
        message = "";
      }
      {
        assertion = !( config.modules.wm.yabai.enable);
        message = "We cannot enable both yabai and amethyst";

      }
    ];
 # brew = {
 #   casks = [
 #     "amethyst"
 #   ];
 # };

  };
}
