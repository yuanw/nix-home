{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.terminal;
in
{

  # error: infinite recursion encountered
  # imports = [ nix-colors.homeManagerModule ];
  options.modules.terminal = {
    enable = mkEnableOption "terminal";
  };

  config = mkIf cfg.enable {

    # colorScheme = nix-colors.colorSchemes.dracula;

    home-manager.users.${config.my.username} = {
      home = {
        file."reiryoku.svg".source = ../../pictures/reiryoku.svg;
        packages = [
          pkgs.jo
          (pkgs.writeShellScriptBin "jsonify" ''
            cat $1 | jq | sponge $1
          '')
        ];
      };
      catppuccin = {
        starship.enable = true;
        bat.enable = true;
        alacritty.enable = true;
        tmux.enable = true;
      };
      programs = {
        pet = {
          enable = true;
          snippets = [
            {
              description = "update a flake source";
              command = "nix flake lock --update-input <input=nixpkgs>";
              tag = [ "nix" ];
            }
            {
              description = "Clean up your system profile";
              command = "sudo nix-collect-garbage --delete-older-than <day=7>d";
              tag = [ "nix" ];
            }

            {
              description = "Clean up your system profile";
              command = "sudo nix-store --optimise";
              tag = [ "nix" ];
            }

            {
              description = "combine pdf";
              command = "gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=merged.pdf source1.pdf source2.pdf source3.pdf";
              tag = [ "pdf" ];
            }
            {
              description = "restart emacs user agent service in macos";
              command = "sudo launchctl kickstart -k gui/$UID/org.nixos.emacs";
              tag = [
                "emacs"
                "macos"
              ];
            }
            {
              description = "check all services";
              command = "checkin my services";
              tag = [ "slack" ];
            }
            {
              description = "check all services";
              command = "checkin content-management-service";
              tag = [ "slack" ];
            }

          ];
        };
        starship = {
          enable = true;
          enableZshIntegration = true;
          # https://starship.rs/config/#prompt
          settings = {
            aws = {
              disabled = true;
            };
            gcloud = {
              disabled = true;
            };
            git_status = {
              ahead = "⇡($count)";
              diverged = "⇕⇡($ahead_count)⇣($behind_count)";
              behind = "⇣($count)";
              modified = "!($count)";
              staged = "[++($count)](green)";
            };
          };
        };

        # https://github.com/alacritty/alacritty/blob/master/alacritty.yml#L1
        alacritty = {
          enable = true;
          settings = {
            font = {
              normal = {
                family = config.my.monoFont;
                style = "Regular";
              };
              bold = {
                family = config.my.monoFont;
                style = "Bold";
              };
              italic = {
                family = config.my.monoFont;
                style = "Italic";
              };
              size = 18;
            };
            cursor.style = "Beam";
            # keyboard.bindings = [
            #   # { key = "Space";  mods= "Control";                 action= "ToggleViMode";            }
            # ];
            window.padding = {
              x = 12;
              y = 0;
            };
            window.decorations = "none";
            # window.dynamic_padding = false;
            # https://github.com/eendroroy/alacritty-theme/blob/master/themes/palenight.yml
          };
        };
      };
    };
  };
}
