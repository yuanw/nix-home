{ pkgs, lib, config, localConfig, ... }:

{
  home.username = localConfig.username;
  home.homeDirectory = localConfig.homeDirectory;
  home.stateVersion = "20.09";
  home.packages = (import ./modules/packages.nix { inherit pkgs; })
    ++ lib.optionals pkgs.stdenvNoCC.isDarwin
    (import ./modules/macos_packages.nix { inherit pkgs; })
    ++ lib.optionals pkgs.stdenvNoCC.isLinux
    (import ./modules/linux_packages.nix { inherit pkgs; });

  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };

  programs = {
    alacritty = {
      enable = true;
      settings = {
        font = {
          normal = {
            family = "PragmataPro";
            style = "Regular";
          };
          bold = {
            family = "PragmataPro";
            style = "Bold";
          };
          italic = {
            family = "PragmataPro";
            style = "Italic";
          };
          size = 18;
        };
        cursor.style = "Beam";
        # background_opacity = 0.8;
        # https://github.com/dracula/alacritty/blob/master/dracula.yml
        colors = {
          # Default colors
          primary = {
            background = "0x292d3e";
            foreground = "0x959dcb";
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
            white = "0x959dcb";
          };
          # Bright colors
          bright = {
            black = "0x676e95";
            red = "0xf07178";
            green = "0xc3e88d";
            yellow = "0xffcb6b";
            blue = "0x82aaff";
            magenta = "0xc792ea";
            cyan = "0x89ddff";
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
    bat = {
      enable = true;
      config.theme = "palenight";
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNixDirenvIntegration = true;
    };

    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      userName = localConfig.username;

      aliases = {
        co = "checkout";
        w = "status -sb";
        l = "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
      };
      userEmail = localConfig.email;

      signing = {
        key = localConfig.gpgKey;
        signByDefault = true;
      };
      ignores = [ ".direnv" ".DS_Store" ];
      extraConfig = {
        core = {
          editor = "emacsclient -a '' -c";
          pager =
            "${pkgs.gitAndTools.delta}/bin/delta --plus-color=\"#012800\" --minus-color=\"#340001\" --theme='ansi-dark'";
        };
        branch.autosetupmerge = true;
      };
    };

    go = { enable = true; };
    gpg = { enable = true; };

    home-manager = { enable = true; };

    jq = { enable = true; };
    starship = {
      enable = true;
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

    tmux = {
      enable = true;
      terminal = "screen-256color";
      clock24 = true;
      escapeTime = 1;
      keyMode = "vi";
      shortcut = "a";

      extraConfig = ''
        unbind -
        bind \| split-window -h
        bind - split-window
      '';
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
    zsh = rec {
      enable = true;
      dotDir = ".config/zsh";

      sessionVariables = {
        PLANTUML_JAR_PATH = "${pkgs.plantuml}/lib/plantuml.jar";
        ASPELL_CONF = "data-dir ${pkgs.aspell}";
        LANG = "en_US.UTF-8";
        GITSTATUS_LOG_LEVEL = "DEBUG";
        EDITOR = "emacs";
      };

      enableAutosuggestions = true;
      history = {
        size = 50000;
        save = 500000;
        path = "$HOME/.config/zsh/history";
        ignoreDups = true;
        share = true;
      };

      initExtra = if pkgs.stdenvNoCC.isDarwin then
        lib.mkBefore ''
          export PATH=$PATH:/usr/local/bin:/usr/local/sbin/:$HOME/.local/bin
          . $HOME/.nix-profile/etc/profile.d/nix.sh
        ''
      else
        lib.mkBefore "";

      oh-my-zsh = {
        enable = true;
        plugins = [ "git" "history" "autojump" "history-substring-search" ];
        custom = "$HOME/.config/zsh/custom";
      };
    };
  };
}
