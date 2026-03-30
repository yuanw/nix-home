{
  config,
  lib,
  pkgs,
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
    enableCozempic = lib.mkEnableOption "cozempic";
  };
  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        agnix
      ];
      mcp-servers.programs.sequential-thinking.enable = true;

      programs.mcp.enable = true;
      # When a background Bash task (run_in_background: true) hangs due to an external dependency failure, Claude Code respawns the command infinitely, creating a fork bomb. In our case this consumed 1,300+ bash processes, 41GB of 64GB RAM, and would have triggered the Linux OOM killer and required a hard reboot if not caught manually.

      # https://github.com/anthropics/claude-code/issues/37490
      programs.zsh.profileExtra = lib.mkAfter ''
        export CLAUDE_CODE_DISABLE_BACKGROUND_TASKS=1
      '';
      programs.claude-code = {
        enable = true;
        enableMcpIntegration = true;
        package = pkgs.llm-agents.claude-code;
        skillsDir = ./skills;
        #commandsDir = ./commands;
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
          ++ lib.optional cfg.enableCozempic [
            cozempic
          ]
          ++ [
            code-review
            commit-commands
            explanatory-output-style
            frontend-design
            learning-output-style
            pr-review-toolkit
            security-guidance
          ];
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
          hooks = import ./hooks/notification.nix { inherit pkgs lib; };
        };
      };
    };
  };
}
