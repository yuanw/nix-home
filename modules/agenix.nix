{ config, lib, options, pkgs, ... }:
let

  cfg = config.modules.secrets.agenix;

in with lib;
with builtins; {

  options = {
    modules.secrets.agenix = {
      enable = mkEnableOption "agenix";

    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      environment.systemPackages = with pkgs; [ agenix rage ];
    }

    (if (builtins.hasAttr "launchd" options) then {
      launchd.daemons.activate-agenix.serviceConfig = {
        StandardOutPath = "/tmp/agenix.out.log";
        StandardErrorPath = "/tmp/agenix.err.log";
      };
    } else
      {
        # systemd
      })

    {
      age = {
        secrets.secret1 = {
          file = ../secrets/secret1.age;
          mode = "770";
          owner = "yuanwang";
          group = "admin";
        };
        identityPaths = options.age.identityPaths.default
          ++ (filter pathExists [
            "${config.my.homeDirectory}/.ssh/id_ed25519"
            "${config.my.homeDirectory}/.ssh/id_rsa"
          ]);

      };
      home-manager.users.${config.my.username} = {

        programs = {
          zsh = {
            sessionVariables = {
              SECRET_PATH = "${config.age.secrets.secret1.path}";
            };
          };
        };
      };
    }

  ]);
}
