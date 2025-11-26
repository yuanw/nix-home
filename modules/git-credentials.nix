{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.git-credentials;
in
{
  options.modules.git-credentials = {
    enable = lib.mkEnableOption "git credential helper";

    helper = lib.mkOption {
      type = lib.types.enum [
        "oauth"
        "manager"
        "libsecret"
        "pass"
      ];
      default = "oauth";
      description = ''
        Which credential helper to use:
        - oauth: git-credential-oauth (recommended for GitHub/GitLab)
        - manager: git-credential-manager (cross-platform, requires .NET)
        - libsecret: use GNOME keyring / libsecret
        - pass: use pass password manager
      '';
    };

    passMapping = lib.mkOption {
      type = lib.types.lines;
      default = ''
        [github.com*]
        target=github
      '';
      description = "Mapping configuration for pass-git-helper";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages =
        lib.optionals (cfg.helper == "oauth") [ pkgs.git-credential-oauth ]
        ++ lib.optionals (cfg.helper == "manager") [ pkgs.git-credential-manager ]
        ++ lib.optionals (cfg.helper == "pass") [ pkgs.pass-git-helper ];

      home.file = lib.mkIf (cfg.helper == "pass") {
        ".config/pass-git-helper/git-pass-mapping.ini".text = cfg.passMapping;
      };

      programs = {
        password-store = lib.mkIf (cfg.helper == "pass") {
          enable = true;
        };

        git.extraConfig = {
          credential.helper =
            if cfg.helper == "oauth" then
              "${pkgs.git-credential-oauth}/bin/git-credential-oauth"
            else if cfg.helper == "manager" then
              "${pkgs.git-credential-manager}/bin/git-credential-manager"
            else if cfg.helper == "libsecret" then
              "${pkgs.git.override { withLibsecret = true; }}/bin/git-credential-libsecret"
            else if cfg.helper == "pass" then
              "${pkgs.pass-git-helper}/bin/pass-git-helper"
            else
              null;
        };
      };
    };
  };
}
