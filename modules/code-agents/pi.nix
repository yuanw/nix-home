{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.modules.pi;
  piExtensions = pkgs.callPackage ../../../packages/pi-extensions { };
  defaultConfigDir = ".pi/agent";

  mkEntries =
    items: nameOf: sourceOf:
    map (item: lib.nameValuePair "${cfg.configDir}/${nameOf item}" { source = (sourceOf item); }) items;

  mkNoDuplicateAssertion =
    values: entityKind:
    let
      duplicates = lib.filter (value: lib.count (x: x == value) values > 1) (lib.unique values);
      mkMsg = value: "  - ${entityKind} `${toString value}`";
    in
    {
      assertion = duplicates == [ ];
      message = ''
        Must not have duplicate ${entityKind}s:
      ''
      + lib.concatStringsSep "\n" (map mkMsg duplicates);
    };
in
{
  options.modules.pi = {
    enable = lib.mkEnableOption "pi";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.llm-agents.pi;
    };

    # Convenience read-only attribute exposing the bundled pi-extensions packages.
    # Usage in host config: config.modules.pi.pkgs.pi-loop
    pkgs = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = piExtensions;
      readOnly = true;
      description = "Bundled pi extension packages from packages/pi-extensions.";
    };

    configDir = lib.mkOption {
      type = lib.types.str;
      default = defaultConfigDir;
      example = ".config/pi/agent";
      description = ''
        Directory where pi agent files (extensions, skills, themes) are stored,
        relative to the home directory.
      '';
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      example = lib.literalExpression ''
        {
          PI_SKIP_VERSION_CHECK = "1";
        }
      '';
      description = "Extra environment variables to set for pi.";
    };

    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Pi extension packages. Each package's pname is used as the extension
        directory name under <configDir>/extensions/.
      '';
    };

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
      description = ''
        Pi skill packages. Each package's pname is used as the skill directory
        name under <configDir>/skills/.
      '';
    };

    themes = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule {
          options.src = lib.mkOption {
            type = lib.types.path;
            description = "Path to the theme JSON file.";
          };
        }
      );
      default = { };
      description = "Custom themes to install under <configDir>/themes/.";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !(cfg.environment ? PI_CODING_AGENT_DIR);
        message = ''
          modules.pi.environment.PI_CODING_AGENT_DIR is managed by modules.pi.configDir.
          Set modules.pi.configDir instead of PI_CODING_AGENT_DIR.
        '';
      }
      (mkNoDuplicateAssertion (map (p: p.pname) cfg.extensions) "extension")
      (mkNoDuplicateAssertion (map (s: s.pname) cfg.skills) "skill")
    ];

    home-manager.users.${config.my.username} = {
      home.packages = [ cfg.package ];

      home.file = lib.listToAttrs (
        (mkEntries cfg.extensions (ext: "extensions/${ext.pname}") (x: x))
        ++ (mkEntries cfg.skills (skill: "skills/${skill.pname}") (x: x))
        ++ (mkEntries (lib.attrsToList cfg.themes) (t: "themes/${t.name}.json") (t: t.value.src))
      );

      home.sessionVariables =
        cfg.environment
        // lib.optionalAttrs (cfg.configDir != defaultConfigDir) {
          PI_CODING_AGENT_DIR = "$HOME/${cfg.configDir}";
        };
    };
  };
}
