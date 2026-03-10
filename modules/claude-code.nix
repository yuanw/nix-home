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
      plugins = [ claudePlugins.claude-mem ];
      settings = {
        alwaysThinkingEnabled = true;
        enabledPlugins = {
          "code-review@claude-code-plugins" = true;
          "commit-commands@claude-code-plugins" = true;
          "explanatory-output-style@claude-code-plugins" = true;
          "frontend-design@claude-code-plugins" = true;
          "learning-output-style@claude-code-plugins" = true;
          "pr-review-toolkit@claude-code-plugins" = true;
          "security-guidance@claude-code-plugins" = true;
          # claude-mem is auto-added via programs.claude-code.plugins
        };
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
