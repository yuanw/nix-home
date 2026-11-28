# Vicinae desktop launcher (https://docs.vicinae.com/nixos).
#
# Wraps upstream's NixOS module (vicinae.nixosModules.default, which sets up
# the privileged vicinae-input-server wrapper for pasting/snippets on Linux)
# and Home Manager module (vicinae.homeManagerModules.default, wired into
# home-manager.sharedModules).
#
# The vicinae flake input intentionally does not follow this repo's nixpkgs so
# builds hit https://vicinae.cachix.org (configured in modules/nix-custom-conf.nix).
# Set `modules.vicinae.package = pkgs.vicinae` to track nixpkgs instead.
{
  config,
  lib,
  pkgs,
  inputs,
  isDarwin,
  isNixOS,
  ...
}:
let
  cfg = config.modules.vicinae;

  jsonFormat = pkgs.formats.json { };
  tomlFormat = pkgs.formats.toml { };
  envVarType =
    with lib.types;
    attrsOf (oneOf [
      str
      int
      float
      bool
    ]);

  extensionPkgs = inputs.vicinae-extensions.packages.${pkgs.stdenv.hostPlatform.system};
  passCopyExtension = import ./extensions/pass-copy { inherit lib pkgs; };
  resolvedExtensions =
    map (ext: if builtins.isString ext then extensionPkgs.${ext} else ext) cfg.extensions
    ++ lib.optional cfg.passExtension.enable passCopyExtension;
in
{
  imports = lib.optionals isNixOS [ inputs.vicinae.nixosModules.default ];

  options.modules.vicinae = {
    enable = lib.mkEnableOption "Vicinae desktop launcher";

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      defaultText = lib.literalExpression "inputs.vicinae.packages.\${system}.default";
      description = ''
        Vicinae package to install. Leave null to use the cached package from
        the upstream vicinae flake (or set `pkgs.vicinae` to use nixpkgs).
      '';
    };

    inputServer.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Enable Vicinae's privileged input server on NixOS (required for
        clipboard/emoji pasting and snippets). Ignored on Darwin.
      '';
    };

    localAI.enable = lib.mkOption {
      type = lib.types.nullOr lib.types.bool;
      default = null;
      description = ''
        Whether to use the `with-local-ai` flake package. When null, upstream
        enables it automatically if `settings.providers.dictation.preferences.model`
        starts with `"local:"`.
      '';
    };

    enableNumen = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable the bundled Numen calculator backend.";
    };

    enableSoulver = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Enable the unfree SoulverCore calculator backend (mutually exclusive
        with `enableNumen`).
      '';
    };

    enableFirefoxIntegration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install the Firefox native messaging host for the Vicinae browser extension.";
    };

    enableChromeIntegration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Install the Chrome/Chromium native messaging host for the Vicinae browser extension.";
    };

    passExtension.enable = lib.mkEnableOption ''
      local pass-copy Vicinae extension (select GNU pass entries and copy to
      clipboard via `pass -c`)
    '';

    systemd = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = isNixOS;
        description = "Run the Vicinae server as a systemd user service on NixOS.";
      };

      autoStart = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Start the Vicinae systemd user service automatically with `systemd.target`.";
      };

      target = lib.mkOption {
        type = lib.types.str;
        default = "graphical-session.target";
        example = "sway-session.target";
        description = "Systemd user target that wants and orders the Vicinae daemon.";
      };

      environment = lib.mkOption {
        type = envVarType;
        default = { };
        example = lib.literalExpression ''
          {
            USE_LAYER_SHELL = 1;
          }
        '';
        description = "Environment variables passed to the Vicinae systemd user service.";
      };
    };

    launchd = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = isDarwin;
        description = "Run the Vicinae server as a launchd user agent on macOS.";
      };

      autoStart = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Start the Vicinae launchd agent automatically at login on macOS.";
      };

      environment = lib.mkOption {
        type = envVarType;
        default = { };
        description = "Environment variables passed to the Vicinae launchd agent on macOS.";
      };
    };

    extensions = lib.mkOption {
      type = lib.types.listOf (
        lib.types.oneOf [
          lib.types.package
          lib.types.str
        ]
      );
      default = [ ];
      example = [
        "nix"
        "power-profile"
      ];
      description = ''
        Vicinae extensions to install. String entries are resolved from
        `inputs.vicinae-extensions.packages.<system>`; derivations built with
        `mkVicinaeExtension` / `mkRayCastExtension` are passed through directly.
      '';
    };

    themes = lib.mkOption {
      inherit (tomlFormat) type;
      default = { };
      description = ''
        Custom Vicinae themes written to `~/.local/share/vicinae/themes/<name>.toml`.
      '';
    };

    settingOverrides = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [ ];
      example = lib.literalExpression ''
        [ config.age.secrets.vicinae-secrets.path ]
      '';
      description = ''
        Extra JSON files merged onto Vicinae's settings via `VICINAE_OVERRIDES`
        (useful for agenix/sops secret files outside the Nix store).
      '';
    };

    settings = lib.mkOption {
      inherit (jsonFormat) type;
      default = { };
      example = lib.literalExpression ''
        {
          close_on_focus_loss = true;
          consider_preedit = true;
          pop_to_root_on_close = true;
          favicon_service = "twenty";
          search_files_in_root = true;
          font.normal = {
            size = 12;
            family = "Maple Nerd Font";
          };
          theme = {
            light = {
              name = "vicinae-light";
              icon_theme = "default";
            };
            dark = {
              name = "vicinae-dark";
              icon_theme = "default";
            };
          };
          launcher_window.opacity = 0.98;
        }
      '';
      description = ''
        Declarative Vicinae settings passed to `programs.vicinae.settings`
        (applied via `VICINAE_OVERRIDES`).
      '';
    };
  };

  config = lib.mkMerge [
    (lib.optionalAttrs isNixOS {
      # Upstream's nixosModules.default defaults input-server.enable to true,
      # which would pull Vicinae into every NixOS host even when disabled here.
      programs.vicinae.input-server = {
        enable = lib.mkDefault (cfg.enable && cfg.inputServer.enable);
        package = lib.mkIf (cfg.package != null) cfg.package;
      };
    })

    (lib.mkIf cfg.enable {
      home-manager.users.${config.my.username} = {
        programs.vicinae = {
          enable = true;
          package = cfg.package;
          enableNumen = cfg.enableNumen;
          enableSoulver = cfg.enableSoulver;
          enableFirefoxIntegration = cfg.enableFirefoxIntegration;
          enableChromeIntegration = cfg.enableChromeIntegration;
          extensions = resolvedExtensions;
          themes = cfg.themes;
          settingOverrides = cfg.settingOverrides;
          settings = cfg.settings;
          systemd = lib.mkIf isNixOS {
            enable = cfg.systemd.enable;
            autoStart = cfg.systemd.autoStart;
            target = cfg.systemd.target;
            environment = cfg.systemd.environment;
          };
          launchd = lib.mkIf isDarwin {
            enable = cfg.launchd.enable;
            autoStart = cfg.launchd.autoStart;
            environment = cfg.launchd.environment;
          };
        }
        // lib.optionalAttrs (cfg.localAI.enable != null) {
          localAI.enable = cfg.localAI.enable;
        };
      };
    })
  ];
}
