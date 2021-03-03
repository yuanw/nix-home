{
  description = "A very basic flake";

  inputs = {
    # TODO: Move to 20.09 when stdenv fix on Big Sur is backported
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    #nixpkgs.url = "github:nixos/nixpkgs";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    # TODO: Move back to release branch when msmtp passwordCommand no longer
    #       uses appended 'echo'
    home-manager.url = "github:nix-community/home-manager/release-20.09";
    #home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR";
    emacs.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, darwin, home-manager, nur, emacs }:
    let
      fullName = "Yuan Wang";
      config = { lib, config, pkgs, ... }:
        with pkgs.stdenv;
        with lib; {
          nix.package = pkgs.nixFlakes;
          nix.extraOptions = ''
            experimental-features = nix-command flakes
          '';

          system.stateVersion = 4;
          nix.maxJobs = 8;
          services.nix-daemon.enable = false;

          nixpkgs.overlays = [ nur.overlay emacs.overlay ];
          nixpkgs.config.allowUnfree = true;

          environment.shells = [ pkgs.zsh ];
          environment.systemPackages = [ pkgs.zsh pkgs.gcc ];
          programs.bash.enable = false;
          programs.zsh.enable = true;

          users.users.yuanwang.shell = pkgs.zsh;
          users.users.yuanwang.home = "/Users/yuanwang";

          system.defaults = {
            dock = {
              autohide = true;
              mru-spaces = false;
              minimize-to-application = true;
            };

            screencapture.location = "/tmp";

            finder = {
              AppleShowAllExtensions = true;
              _FXShowPosixPathInTitle = true;
              FXEnableExtensionChangeWarning = false;
            };

            trackpad = {
              Clicking = true;
              TrackpadThreeFingerDrag = true;
            };

            NSGlobalDomain._HIHideMenuBar = true;
          };

          fonts.enableFontDir = true;
          fonts.fonts = with pkgs; [
            emacs-all-the-icons-fonts
            fira-code
            font-awesome
            roboto
            roboto-mono
          ];

          system.keyboard = {
            enableKeyMapping = true;
            remapCapsLockToControl = true;
          };

          services.yabai = {
            enable = true;
            package = pkgs.yabai;
            enableScriptingAddition = true;
            config = {
              window_border = "on";
              window_border_width = 5;
              active_window_border_color = "0xff81a1c1";
              normal_window_border_color = "0xff3b4252";
              focus_follows_mouse = "autoraise";
              mouse_follows_focus = "off";
              window_placement = "second_child";
              window_opacity = "off";
              window_topmost = "on";
              window_shadow = "float";
              active_window_opacity = "1.0";
              normal_window_opacity = "1.0";
              split_ratio = "0.50";
              auto_balance = "on";
              mouse_modifier = "fn";
              mouse_action1 = "move";
              mouse_action2 = "resize";
              layout = "bsp";
              top_padding = 10;
              bottom_padding = 10;
              left_padding = 10;
              right_padding = 10;
              window_gap = 10;
              external_bar = "all:26:0";
            };

            extraConfig = mkDefault ''
              # rules
              yabai -m rule --add app='System Preferences' manage=off
              yabai -m rule --add app='Live' manage=off
            '';
          };

          services.spacebar.enable = true;
          services.spacebar.package = pkgs.spacebar;
          services.spacebar.config = {
            debug_output = "on";
            position = "top";
            clock_format = "%R";
            space_icon_strip = "   ";
            text_font = ''"Roboto Mono:Regular:12.0"'';
            icon_font = ''"FontAwesome:Regular:12.0"'';
            background_color = "0xff2e3440";
            foreground_color = "0xffd8dee9";
            space_icon_color = "0xff81a1c1";
            dnd_icon_color = "0xff81a1c1";
            clock_icon_color = "0xff81a1c1";
            power_icon_color = "0xff81a1c1";
            battery_icon_color = "0xff81a1c1";
            power_icon_strip = " ";
            space_icon = "";
            clock_icon = "";
            dnd_icon = "";
          };

          # Recreate /run/current-system symlink after boot
          services.activate-system.enable = true;

          home-manager.users.yuanwang = {
            home.stateVersion = "20.09";
            home.packages = with pkgs; [
              aspell
              aspellDicts.en
              aspellDicts.en-computers
              bc
              bind
              clang
              ffmpeg-full
              gnumake
              gnupg
              gnused
              htop
              hugo
              jq
              mpv
              nixops
              nix-prefetch-git
              nmap
              open-policy-agent
              pass
              python3
              ranger
              ripgrep
              rsync
              terraform
              unzip
              up
              vim
              wget
              wireguard-tools
              youtube-dl

              # Go
              go
              gocode
              godef
              gotools
              golangci-lint
              golint
              go2nix
              errcheck
              gotags
              gopls

              # Docker
              docker

              # k8s
              argocd
              kind
              kubectl
              kubectx
              kubeval
              kube-prompt
              kubernetes-helm
              kustomize
            ];

            home.sessionVariables = {
              PAGER = "less -R";
              EDITOR = "emacsclient";
            };

            programs.fzf.enable = true;
            programs.fzf.enableZshIntegration = true;

            programs.alacritty = {
              enable = true;
              settings = {
                window.padding.x = 24;
                window.padding.y = 24;
                window.decorations = "buttonless";
                window.dynamic_title = true;
                scrolling.history = 100000;
                live_config_reload = true;
                selection.save_to_clipboard = true;
                mouse.hide_when_typing = true;
                use_thin_strokes = true;

                font = {
                  size = 12;
                  normal.family = "Roboto Mono";
                };

                colors = {
                  cursor.cursor = "#81a1c1";
                  primary.background = "#2e3440";
                  primary.foreground = "#d8dee9";

                  normal = {
                    black = "#3b4252";
                    red = "#bf616a";
                    green = "#a3be8c";
                    yellow = "#ebcb8b";
                    blue = "#81a1c1";
                    magenta = "#b48ead";
                    cyan = "#88c0d0";
                    white = "#e5e9f0";
                  };

                  bright = {
                    black = "#4c566a";
                    red = "#bf616a";
                    green = "#a3be8c";
                    yellow = "#ebcb8b";
                    blue = "#81a1c1";
                    magenta = "#b48ead";
                    cyan = "#8fbcbb";
                    white = "#eceff4";
                  };
                };

                key_bindings = [
                  {
                    key = "V";
                    mods = "Command";
                    action = "Paste";
                  }
                  {
                    key = "C";
                    mods = "Command";
                    action = "Copy";
                  }
                  {
                    key = "Q";
                    mods = "Command";
                    action = "Quit";
                  }
                  {
                    key = "Q";
                    mods = "Control";
                    chars = "\\x11";
                  }
                  {
                    key = "F";
                    mods = "Alt";
                    chars = "\\x1bf";
                  }
                  {
                    key = "B";
                    mods = "Alt";
                    chars = "\\x1bb";
                  }
                  {
                    key = "D";
                    mods = "Alt";
                    chars = "\\x1bd";
                  }
                  {
                    key = "Slash";
                    mods = "Control";
                    chars = "\\x1f";
                  }
                  {
                    key = "Period";
                    mods = "Alt";
                    chars = "\\e-\\e.";
                  }
                  {
                    key = "N";
                    mods = "Command";
                    command = {
                      program = "open";
                      args = [ "-nb" "io.alacritty" ];
                    };
                  }
                ];
              };
            };

            programs.zsh = {
              enable = true;
              enableAutosuggestions = true;
              enableCompletion = true;
              defaultKeymap = "emacs";
              sessionVariables = { RPROMPT = ""; };

              shellAliases = {
                k = "kubectl";
                kp = "kube-prompt";
                kc = "kubectx";
                kn = "kubens";
                t = "cd $(mktemp -d)";
              };

              oh-my-zsh.enable = true;
            };

          };
        };
    in
    {
      darwinConfigurations."wf17084" = darwin.lib.darwinSystem {
        modules = [ home-manager.darwinModules.home-manager ];
        # inputs = { inherit darwin nixpkgs home-manager; };
      };
    };
}
