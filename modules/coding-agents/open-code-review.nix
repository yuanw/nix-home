{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.open-code-review;
in
{
  options.modules.open-code-review = {
    enable = lib.mkEnableOption "open-code-review (ocr)";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.open-code-review;
      description = "Which open-code-review package to use; provides the `ocr` command.";
    };

    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      example = lib.literalExpression ''
        {
          provider = "ollama";
          model = "qwen3:32b";
          custom_providers.ollama = {
            url = "http://127.0.0.1:11434/v1";
            protocol = "openai";
            model = "qwen3:32b";
            api_key = "ollama";
          };
        }
      '';
      description = ''
        Attributes merged into ~/.opencodereview/config.json, see
        https://open-codereview.ai/docs/configuration. Existing keys written by
        `ocr config` are kept; leave empty to manage no config file at all.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ cfg.package ];

      mergetools = lib.mkIf (cfg.settings != { }) {
        open-code-review = {
          target = "${config.my.homeDirectory}/.opencodereview/config.json";
          format = "json";
          force = true;
          settings = cfg.settings;
        };
      };
    };
  };
}
