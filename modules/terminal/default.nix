{ config, lib, pkgs, ... }:

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
      programs = {
        pet = {
          enable = true;
          snippets = [
            {
              description = "update a flake source";
              command = " ix flake lock --update-input <input=nixpkgs>";
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
              tag = [ "emacs" "macos" ];
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
          catppuccin.enable = true;

          enableZshIntegration = true;
          # https://starship.rs/config/#prompt
          settings = {
            aws = { disabled = true; };
            gcloud = { disabled = true; };
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
                family = config.my.font;
                style = "Regular";
              };
              bold = {
                family = config.my.font;
                style = "Bold";
              };
              italic = {
                family = config.my.font;
                style = "Italic";
              };
              size = 18;
            };
            cursor.style = "Beam";
            keyboard.bindings = [
              # { key = "Space";  mods= "Control";                 action= "ToggleViMode";            }
            ];
            window.padding = {
              x = 12;
              y = 0;
            };
            window.decorations = "none";
            # window.dynamic_padding = false;
            # https://github.com/eendroroy/alacritty-theme/blob/master/themes/palenight.yml
            colors = {
              # Default colors
              primary = {
                background = "0x292d3e";
                foreground = "0xd0d0d0";
              };
              cursor = {
                text = "0x202331";
                cursor = "0xc792ea";
              };
              # Normal colors
              normal = {
                black = "0x292d3e";
                red = "0xf07178";
                green = "0xc3e88d";
                yellow = "0xffcb6b";
                blue = "0x82aaff";
                magenta = "0xc792ea";
                cyan = "0x89ddff";
                white = "0xd0d0d0";
              };
              # Bright colors
              bright = {
                black = "0x434758";
                red = "0xff8b92";
                green = "0xddffa7";
                yellow = "0xffe585";
                blue = "0x9cc4ff";
                magenta = "0xe1acff";
                cyan = "0xa3f7ff";
                white = "0xffffff";
              };
              indexed_colors = [
                {
                  index = 16;
                  color = "0xf78c6c";
                }
                {
                  index = 17;
                  color = "0xff5370";
                }
                {
                  index = 18;
                  color = "0x444267";
                }
                {
                  index = 19;
                  color = "0x32374d";
                }
                {
                  index = 20;
                  color = "0x8796b0";
                }
                {
                  index = 21;
                  color = "0x959dcb";
                }
              ];
            };
          };
        };
      };
    };
  };
}
