{ config, lib, pkgs, ... }:
let
  homeDir = builtins.getEnv ("HOME");
  configDir = ../conf.d;
in with pkgs.stdenv;
with lib; {

  imports = [ ./module-list ];
  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = [ ];

  environment.etc.hosts.enable = true;
  environment.etc.hosts.text = let
    hostsPath =
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/alternates/fakenews-gambling/hosts";
    hostsFile = builtins.fetchurl hostsPath;
  in builtins.readFile "${hostsFile}";

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
    overlays = let path = ../overlays;
    in with builtins;
    map (n: import (path + ("/" + n))) (filter (n:
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
      pkgs.emacs-all-the-icons-fonts
      pkgs.pragmata-pro-font
    ];
  };

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;
  services.lorri = { enable = true; };
  services.skhd = {
    enable = true;
    skhdConfig = ''
      # launchers
      cmd - e : open ~/.nix-profile/Applications/Emacs.app
      cmd + ctrl - return : kitty --single-instance
      cmd + ctrl - v: osascript -e 'tell application "Viscosity" to connect "work"'

      # focus window
      cmd + ctrl - h : yabai -m window --focus west
      cmd + ctrl - j : yabai -m window --focus south || yabai -m display --focus prev
      cmd + ctrl - k : yabai -m window --focus north || yabai -m display --focus next
      cmd + ctrl - l : yabai -m window --focus east
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
    text_font = ''"Essential PragmataPro:Regular:12.0"'';
    icon_font = ''"FontAwesome:Regular:12.0"'';
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
    enableScriptingAddition = true;
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
      minimize-to-application = true;
      show-process-indicators = true;
    };

    screencapture.location = "/tmp";

    finder = {
      AppleShowAllExtensions = true;
      _FXShowPosixPathInTitle = true;
      FXEnableExtensionChangeWarning = false;
    };

    trackpad = {
      Clicking = false;
      TrackpadThreeFingerDrag = true;
    };

    NSGlobalDomain._HIHideMenuBar = true;
  };

  home-manager.users.yuanwang = {
    home.packages = (import ./packages.nix { inherit pkgs; });

    home.file = {
      ".ghci".text = ''
        :set prompt "λ> "
      '';

      ".config/zsh/custom/plugins/iterm2/iterm2.plugin.zsh".source =
        pkgs.fetchurl {
          url = "https://iterm2.com/shell_integration/zsh";
          sha256 = "1gw3rk0dsss3vl92wxpda7br8gmwrx6jk41xm3i3rh6p2d7r97z0";
          # date = 2020-01-07T15:59:09-0800;
        };

      ".config/kitty/dracula.conf".source =
        lib.cleanSource ../conf.d/kitty/dracula.conf;

      ".doom.d".source = configDir + "/doom";
    };

    programs = {
      direnv = {
        enable = true;
        enableZshIntegration = true;
        enableNixDirenvIntegration = true;
      };

      emacs = {
        enable = true;
        package = pkgs.emacsMacport;
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
            editor = "${pkgs.emacsMacport}/bin/emacsclient -a '' -c";
            pager =
              "${pkgs.gitAndTools.delta}/bin/delta --plus-color=\"#012800\" --minus-color=\"#340001\" --theme='ansi-dark'";
          };
          branch.autosetupmerge = true;
          credential.helper =
            "${pkgs.gitAndTools.pass-git-helper}/bin/pass-git-helper";
          # "url \"git@github.com:\"".insteadOf = "https://github.com/";
        };
      };

      gpg = { enable = true; };

      home-manager = { enable = true; };

      jq = { enable = true; };

      kitty = {
        enable = true;
        extraConfig = "include dracula.conf";
      };

      #pet = { enable = true; };

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
        };

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
          export PATH=$HOME/.local/bin:$PATH:$GOPATH/bin:$HOME/.emacs.d/bin
          . ${homeDir}/.nix-profile/etc/profile.d/nix.sh

          export NIX_PATH=$NIX_PATH:$HOME/.nix-defexpr/channels

          function prev() {
              PREV=$(fc -lrn | head -n 1)
              sh -c "pet new `printf %q "$PREV"`"
          }

          function bigskyTest {
              python manage.py test $1 --http-integration --traceback -v 2
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
