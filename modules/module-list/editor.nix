{ config, lib, pkgs, ... }:

with lib;
{
  options.modules.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    
    pkg = mkOption {
      type = types.package;
      default = pkgs.emacsMacport;
    };

    enableDoomConfig = mkOption {
      type = types.bool;
      default = false;
    };
  };

  
}
