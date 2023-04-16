{ config, lib, options, pkgs, ... }:
let

  cfg = config.modules.secrets.agenix;
in with lib;
with builtins; {

  options.modules.secrets.agenix = {
    options = {
      enable = mkEnableOption "agenix";
      isDarwin = mkOption { type = types.bool; };
      isNixOS = mkOption { type = types.bool; };

    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      assertions = [{
        assertion = let os = [ cfg.isDarwin cfg.isNixOS ]; in count id os <= 1;
        message =
          "Only one of 'programs.git.difftastic.enable' or 'programs.git.diff-so-fancy.enable' can be set to true at the same time.";
      }];

      environment.systemPackages = with pkgs; [ agenix rage ];
    }

    # (mkIf cfg.isDarwin {
    #   launchd.daemons.activate-agenix.serviceConfig = {
    #     StandardOutPath = "/tmp/agenix.out.log";
    #     StandardErrorPath = "/tmp/agenix.err.log";
    #   };
    # })

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
