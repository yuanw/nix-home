{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.modules.mics-skills;
in
{
  options.modules.mics-skills = {
    enable = lib.mkEnableOption ''
      Mic92's mics-skills (programs.mics-skills): selected skill packages
      linked into agent skills directories.
    '';

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "browser-cli"
        "kagi-search"
        "screenshot-cli"
      ];
      description = "Skill names from the mics-skills package to install.";
    };

    skillDirs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ".pi/agent/skills" ];
      description = "Relative home paths where selected skills are linked.";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      programs.mics-skills = {
        enable = true;
        package = inputs.mics-skills.packages.${pkgs.stdenv.hostPlatform.system};
        skills = cfg.skills;
        skillDirs = cfg.skillDirs;
      };
    };
  };
}
