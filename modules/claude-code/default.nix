{
  config,
  lib,
  pkgs,
  inputs',
  ...
}:
let
  cfg = config.modules.claude-code;
  claudePlugins = pkgs.callPackage ../../packages/claude-plugins { };
in
{
  options.modules.claude-code = {
    enable = lib.mkEnableOption "claude-code";
    enableClaudeMem = lib.mkEnableOption "claude-mem";

  };
  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      programs.claude-code = {
        enable = true;
        package = inputs'.claude-code.packages.claude-code;
        skillsDir = ./skills;
        skillPackages = with claudePlugins; [
          caveman
          humanizer
          emacs-skills
        ];
        plugins =
          with claudePlugins;
          lib.optionals cfg.enableClaudeMem [
            claude-mem
          ]
          ++ [
            cozempic
            code-review
            commit-commands
            explanatory-output-style
            frontend-design
            learning-output-style
            pr-review-toolkit
            security-guidance
          ];
        hooks = import ./hooks/notification.nix { inherit pkgs lib; };
        settings = {
          includeCoAuthoredBy = false;
          alwaysThinkingEnabled = true;
          gitAttribution = false;
          attribution = {
            commit = "";
            pr = "";
          };
          git = {
            includeCoAuthor = false;
            includePRFooter = false;
          };
          # enabledPlugins auto-generated from programs.claude-code.plugins list
        };
      };
    };
  };
}
