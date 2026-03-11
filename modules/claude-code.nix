{
  config,
  lib,
  pkgs,
  inputs',
  ...
}:
let
  claudePlugins = pkgs.callPackage ../packages/claude-plugins { };
in
{
  home-manager.users.${config.my.username} = {
    programs.claude-code = {
      enable = true;
      package = inputs'.claude-code.packages.claude-code;
      plugins = with claudePlugins; [
        claude-mem
        code-review
        commit-commands
        explanatory-output-style
        frontend-design
        learning-output-style
        pr-review-toolkit
        security-guidance
      ];
      settings = {
        alwaysThinkingEnabled = true;
        # enabledPlugins auto-generated from programs.claude-code.plugins list
        hooks = {
          Notification = lib.mkIf pkgs.stdenv.isDarwin [
            {
              matcher = "permission_prompt";
              hooks = [
                {
                  type = "command";
                  command = "osascript -e 'display notification \"⚠️ Permission needed to run command\" with title \"Claude Code - $(basename \"$PWD\")\" subtitle \"$PWD\" sound name \"Ping\"'";
                }
              ];
            }
            {
              matcher = "idle_prompt";
              hooks = [
                {
                  type = "command";
                  command = "osascript -e 'display notification \"💬 Waiting for your input\" with title \"Claude Code - $(basename \"$PWD\")\" subtitle \"$PWD\" sound name \"Purr\"'";
                }
              ];
            }
            {
              matcher = "elicitation_dialog";
              hooks = [
                {
                  type = "command";
                  command = "osascript -e 'display notification \"🤔 Need your decision\" with title \"Claude Code - $(basename \"$PWD\")\" subtitle \"$PWD\" sound name \"Pop\"'";
                }
              ];
            }
            {
              matcher = "auth_success";
              hooks = [
                {
                  type = "command";
                  command = "osascript -e 'display notification \"✅ Authentication successful\" with title \"Claude Code - $(basename \"$PWD\")\" subtitle \"$PWD\" sound name \"Glass\"'";
                }
              ];
            }
          ];
          PostToolUse = [
            {
              matcher = "Write|Edit|MultiEdit";
              hooks = [
                {
                  type = "command";
                  command = "${inputs'.git-ai.packages.default}/bin/git-ai checkpoint claude --hook-input stdin";
                }
              ];
            }
          ];
          PreToolUse = [
            {
              matcher = "Write|Edit|MultiEdit";
              hooks = [
                {
                  type = "command";
                  command = "${inputs'.git-ai.packages.default}/bin/git-ai checkpoint claude --hook-input stdin";
                }
              ];
            }
          ];
        };
      };
    };
  };
}
