{ config, lib, pkgs, packages, ... }:

with lib;
let homeDir = config.my.homeDirectory;
in {
  options.modules.workShell = { enable = mkEnableOption "workivaShell"; };

  config = mkIf config.modules.workShell.enable {

    home-manager.users.${config.my.username} = {
      home.packages = [
        pkgs.kubernetes-helm
        pkgs.aws-iam-authenticator
        pkgs.clang-tools
        # pkgs.antlr4
        packages.resource-id
        # pkgs.csvkit
        # pkgs.visidata
        pkgs.trivy
        pkgs.terraform
        pkgs.terraform-ls
        # pkgs.podman
      ];
      home.sessionPath = [
        "${homeDir}/go/bin"
      ];
      programs.zsh = {
        profileExtra = mkAfter ''
                    [[ -s "${homeDir}/.wk/profile" ]] && source "${homeDir}/.wk/profile"

                    function scan-image {
                       trivy image $1 --scanners vuln
                    }

                    function jwt-decode {
                        jq -R 'split(".") |.[0:2] | map(gsub("-"; "+") | gsub("_"; "/") | gsub("%3D"; "=") | @base64d) | map(fromjson)' <<< $1
          }
        '';
      };

    };
  };
}
