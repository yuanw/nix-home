{ config, lib, pkgs, ... }:
let homeDir = builtins.getEnv ("HOME");
in
with pkgs.stdenv;
with lib; {

  imports = [ ./modules ];
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [ ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin-configuration.nix
  environment.darwinConfig =
    "${homeDir}/.config/nixpkgs/machines/${config.networking.hostName}/configuration.nix";

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true; # default shell on catalina
  programs.bash.enable = false;
  time.timeZone = "America/Regina";
  users.users.yuanwang.shell = pkgs.zsh;
  users.users.yuanwang.home = homeDir;

  nixpkgs = {
    overlays =
      let path = ../overlays;
      in
      with builtins;
      map (n: import (path + ("/" + n))) (filter
        (n:
          match ".*\\.nix" n != null
          || pathExists (path + ("/" + n + "/default.nix")))
        (attrNames (readDir path)));

    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

  };

  # this is computer level Font, there is also user level font. Nix-darwin cannot manage them yet
  fonts = {
    enableFontDir = true;
    fonts = [
      pkgs.material-design-icons
      pkgs.weather-icons
      pkgs.font-awesome
      pkgs.pragmata-pro-font
    ];
  };

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;
  # services.lorri = { enable = true; };
  services.skhd = {
    enable = true;
    skhdConfig = ''
      # launchers
      shift + ctrl + alt - e: open ~/.nix-profile/Applications/Emacs.app
      shift + ctrl + alt - return : open ~/.nix-profile/Applications/Alacritty.app
      shift + ctrl + alt - v: osascript -e 'tell application "Viscosity" to connect "work"'

                   # focus window
                   alt - left: yabai -m window --focus west
                   alt - down : yabai -m window --focus south || yabai -m display --focus prev
                   alt - up : yabai -m window --focus north || yabai -m display --focus next
                   alt - right : yabai -m window --focus east

                   # shift window in current workspace, use the arrow keys
                   alt + shift - left  : yabai -m window --warp west
                   alt + shift - down  : yabai -m window --warp south
                   alt + shift - up    : yabai -m window --warp north
                   alt + shift - right : yabai -m window --warp east

       # fast focus desktop
       cmd + ctrl - tab : yabai -m space --focus recent
       cmd + ctrl - p : yabai -m space --focus prev
       cmd + ctrl - n : yabai -m space --focus next
       cmd + ctrl - 1 : yabai -m space --focus 1
       cmd + ctrl - 2 : yabai -m space --focus 2



       cmd + ctrl - 0x21 : yabai -m window --focus stack.prev # this is [
                   cmd + ctrl - 0x1E : yabai -m window --focus stack.next # this is ]
    '';
  };

  services.spacebar.enable = true;
  services.spacebar.package = pkgs.spacebar;
  services.spacebar.config = {
    debug_output = "on";
    clock_format = "%R";
    space_icon_strip = "I II III IV V";
    text_font = "PragmataPro:Regular:12.0";
    icon_font = "FontAwesome:Regular:12.0";
    background_color = "0xff202020";
    foreground_color = "0xffa8a8a8";
    space_icon_color = "0xff14b1ab";
    dnd_icon_color = "0xfffcf7bb";
    clock_icon_color = "0xff99d8d0";
    power_icon_color = "0xfff69e7b";
    battery_icon_color = "0xffffbcbc";
    power_icon_strip = " ";
    space_icon = "";
    clock_icon = "";
    dnd_icon = "";
  };

  services.yabai = {
    enable = true;
    package = pkgs.yabai;
    enableScriptingAddition = false;
    config = {
      focus_follows_mouse = "autoraise";
      mouse_follows_focus = "off";
      window_placement = "second_child";
      window_opacity = "off";
      window_opacity_duration = "0.0";
      window_border = "on";
      window_border_placement = "inset";
      window_border_width = 2;
      window_border_radius = 3;
      active_window_border_topmost = "off";
      window_topmost = "on";
      window_shadow = "float";
      active_window_border_color = "0xff5c7e81";
      normal_window_border_color = "0xff505050";
      insert_window_border_color = "0xffd75f5f";
      active_window_opacity = "1.0";
      normal_window_opacity = "1.0";
      split_ratio = "0.50";
      auto_balance = "on";
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      layout = "bsp";
      top_padding = 36;
      bottom_padding = 10;
      left_padding = 10;
      right_padding = 10;
      window_gap = 10;
    };

    extraConfig = ''
      # rules
      yabai -m rule --add app='System Preferences' manage=off

      # Any other arbitrary config here
    '';
  };

  system.defaults = {
    dock = {
      autohide = true;
      mru-spaces = false;
      orientation = "left";
      mineffect = "scale";
      showhidden = true;
      launchanim = false;
      show-recents = false;
      minimize-to-application = true;
      show-process-indicators = true;
      #mouse-over-hilite-stack = false;
    };

    screencapture.location = "/tmp";

    finder = {
      AppleShowAllExtensions = true;
      _FXShowPosixPathInTitle = true;
      FXEnableExtensionChangeWarning = false;
    };

    #trackpad = {
    #  Clicking = true;
    #  TrackpadThreeFingerDrag = true;
    #};

    NSGlobalDomain._HIHideMenuBar = true;
    #NSGlobalDomain."com.apple.mouse.tapBehavior" = null;
  };
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToControl = true;
  };

  home-manager.users.yuanwang = {
    home.packages = (import ./packages.nix { inherit pkgs; });

    home.file = {
      ".ghci".text = ''
        :set prompt "λ> "
      '';
    };

    programs = {
      direnv = {
        enable = true;
        enableZshIntegration = true;
        enableNixDirenvIntegration = true;
      };

      git = {
        enable = true;
        userName = "Yuan Wang";

        aliases = {
          co = "checkout";
          w = "status -sb";
          l = "log --graph --pretty=format:'%Cred%h%Creset"
            + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
            + " --abbrev-commit --date=relative --show-notes=*";
        };

        extraConfig = {
          core = {
            editor = "emacs -nw";
            pager =
              "${pkgs.gitAndTools.delta}/bin/delta --plus-color=\"#012800\" --minus-color=\"#340001\" --theme='ansi-dark'";
          };
          branch.autosetupmerge = true;
          credential.helper =
            "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
          # "url \"git@github.com:\"".insteadOf = "https://github.com/";
        };

        ignores = [ ".direnv" ".DS_Store" "org.eclipse.buildship.core.prefs" ];
      };

      gpg = { enable = true; };

      home-manager = { enable = true; };

      jq = { enable = true; };

      alacritty = {
        enable = true;
        settings = {
          font = {
            normal = { family = "PragmataPro"; };
            size = 20;
          };
          # https://github.com/dracula/alacritty/blob/master/dracula.yml
          colors = {
            primary.background = "0x282c34";
            primary.foreground = "0xabb2bf";

            cursor.text = "0x44475a";
            cursor.cursor = "0xf8f8f2";

            selection = {
              text = "0xf8f8f2";
              background = "0x44475a";
            };
            normal = {
              black = "0x000000";
              red = "0xff5555";
              green = "0x50fa7b";
              yellow = "0xf1fa8c";
              blue = "0xbd93f9";
              magenta = "0xff79c6";
              cyan = "0x8be9fd";
              white = "0xbfbfbf";
            };

            bright = {
              black = "0x4d4d4d";
              red = "0xff6e67";
              green = "0x5af78e";
              yellow = "0xf4f99d";
              blue = "0xcaa9fa";
              magenta = "0xff92d0";
              cyan = "0x9aedfe";
              white = "0xe6e6e6";
            };

            dim = {
              black = "0x14151b";
              red = "0xff2222";
              green = "0x1ef956";
              yellow = "0xebf85b";
              blue = "0x4d5b86";
              magenta = "0xff46b0";
              cyan = "0x59dffc";
              white = "0xe6e6d1";
            };

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
          bind-key -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "pbcopy"
          bind C-v run "reattach-to-user-namespace pbpaste | tmux load-buffer - && tmux paste-buffer"

        '';
      };

      zoxide = {
        enable = true;
        enableZshIntegration = true;
      };

      zsh = rec {
        enable = true;
        dotDir = ".config/zsh";
        plugins = [{
          name = "powerlevel10k-config";
          src = lib.cleanSource ../conf.d/p10k-config;
          file = "p10k.zsh";
        }];

        sessionVariables = {
          PLANTUML_JAR_PATH = "${pkgs.plantuml}/lib/plantuml.jar";
          ASPELL_CONF = "data-dir ${pkgs.aspell}";
          LANG = "en_US.UTF-8";
          GITSTATUS_LOG_LEVEL = "DEBUG";
          EDITOR = "emacs";
          SKHD_PATH = "${pkgs.skhd}/bin";
        };

        shellAliases = { alerter = "${pkgs.alerter}/alerter"; };

        enableAutosuggestions = true;
        history = {
          size = 50000;
          save = 500000;
          path = "${dotDir}/history";
          ignoreDups = true;
          share = true;
        };

        initExtraBeforeCompInit =
          "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme";

        initExtra = lib.mkBefore ''
          export PATH=$PATH:/usr/local/bin:/usr/local/sbin
          export PATH=$PATH:$HOME/.local/bin
          . ${homeDir}/.nix-profile/etc/profile.d/nix.sh

          function prev() {
              PREV=$(fc -lrn | head -n 1)
              sh -c "pet new `printf %q "$PREV"`"
          }
        '';

        oh-my-zsh = {
          enable = true;
          plugins = [ "git" "history" "autojump" "history-substring-search" ];
          custom = "$HOME/.config/zsh/custom";
        };
      };
    };
  };
}
