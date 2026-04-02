{
  config,
  lib,
  pkgs,
  inputs',
  ...
}:

with lib;
let
  cfg = config.modules.ai;
in
{
  options.modules.ai = {
    enable = mkEnableOption "ai";
    enableGitAI = mkEnableOption "git-ai";
    enableOllama = mkEnableOption "ollama";
  };

  config = mkMerge [
    (mkIf cfg.enableOllama {
      home-manager.users.${config.my.username} = {
        home.packages = [
          # (pkgs.python3.withPackages (ps:
          #   with ps; [
          #     huggingface-hub
          #   ]))
          pkgs.ollama
          pkgs.llm-agents.opencode
        ];
      };
    })

    (mkIf cfg.enableGitAI {
      home-manager.users.${config.my.username} = {
        programs = {
          git-ai = {
            enable = true;
            installHooks = true;
          };
          git.package = inputs'.git-ai.packages.default;
        };
      };
    })
  ];
}
