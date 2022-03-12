{ pkgs, lib, config, ... }:

{
  home.username = config.my.username;
  home.homeDirectory = config.my.homeDirectory;
  home.stateVersion = "22.05";
  home.packages = (import ./modules/packages.nix { inherit pkgs; })
    ++ lib.optionals pkgs.stdenvNoCC.isDarwin
    (import ./modules/macos_packages.nix { inherit pkgs; })
    ++ lib.optionals pkgs.stdenvNoCC.isLinux
    (import ./modules/linux_packages.nix { inherit pkgs; });

  xdg.enable = true;


  programs = {
    bat = {
      enable = true;
      config.theme = "palenight";
      themes = {
        palenight = builtins.readFile (pkgs.fetchFromGitHub {
          owner = "equinusocio";
          repo = "material-theme";
          rev = "614b7e8bc7369c32e852297d42253643ebf90d55";
          sha256 = "1gjfisksvqa2d08na0yln7yxny4i16wrmvlfnwllbqrgwh26v94g";
        } + "/schemes/Material-Theme-Palenight.tmTheme");
      };
    };
    dircolors = {
      enable = true;
      enableZshIntegration = true;
      # https://github.com/trapd00r/LS_COLORS/blob/master/LS_COLORS
      settings = {
        "BLK" = "38;5;68";
        "CAPABILITY" = "38;5;17";
        "CHR" = "38;5;113;1";
        "DIR" = "38;5;30";
        "DOOR" = "38;5;127";
        "EXEC" = "38;5;208;1";
        "FIFO" = "38;5;126";
        "FILE" = "0";
        "LINK" = "target";
        "MULTIHARDLINK" = "38;5;222;1";
        # "NORMAL don't reset the bold attribute -
        # https://github.com/trapd00r/LS_COLORS/issues/11
        #NORMAL                38;5;254
        "NORMAL" = "0";
        "ORPHAN" = "48;5;196;38;5;232;1";
        "OTHER_WRITABLE" = "38;5;220;1";
        "SETGID" = "48;5;3;38;5;0";
        "SETUID" = "38;5;220;1;3;100;1";
        "SOCK" = "38;5;197";
        "STICKY" = "38;5;86;48;5;234";
        "STICKY_OTHER_WRITABLE" = "48;5;235;38;5;139;3";
        "*LS_COLORS" = "48;5;89;38;5;197;1;3;4;7";
        # }}}
        # documents {{{1
        "*README" = "38;5;220;1";
        "*README.rst" = "38;5;220;1;";
        "*README.md" = "38;5;220;1";
        "*LICENSE" = "38;5;220;1";
        "*COPYING" = "38;5;220;1";
        "*INSTALL" = "38;5;220;1";
        "*COPYRIGHT" = "38;5;220;1";
        "*AUTHORS" = "38;5;220;1";
        "*HISTORY" = "38;5;220;1";
        "*CONTRIBUTORS" = "38;5;220;1";
        "*PATENTS" = "38;5;220;1";
        "*VERSION" = "38;5;220;1";
        "*NOTICE" = "38;5;220;1";
        "*CHANGES" = "38;5;220;1";
        ".log" = "38;5;190";
        # plain-text {{{2
        ".txt" = "38;5;253";
        # markup {{{2
        ".adoc" = "38;5;184";
        ".asciidoc" = "38;5;184";
        ".etx" = "38;5;184";
        ".info" = "38;5;184";
        ".markdown" = "38;5;184";
        ".md" = "38;5;184";
        ".mkd" = "38;5;184";
        ".nfo" = "38;5;184";
        ".pod" = "38;5;184";
        ".rst" = "38;5;184";
        ".tex" = "38;5;184";
        ".textile" = "38;5;184";
        ".java" = "38;5;074;1";
        "*Dockerfile" = "38;5;155";
        ".dockerignore" = "38;5;240";
        "*Makefile" = "38;5;155";
        "*MANIFEST" = "38;5;243";
        # Functional Configuration
        ".nix" = "38;5;155";
        ".dhall" = "38;5;178";
      };
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = { enable = true; };
    };

    exa = {
      enable = true;
      enableAliases = true;
    };
    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      userName = config.my.username;

      aliases = {
        co = "checkout";
        w = "status -sb";
        l = "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
      };
      userEmail = config.my.email;

      delta = { enable = true; };

      signing = {
        key = config.my.gpgKey;
        signByDefault = true;
      };
      ignores = [ ".direnv" ".DS_Store" ".envrc" ];
      extraConfig = {
        core = { editor = "emacsclient -c"; };
        init.defaultBranch = "main";
        pull.rebase = true;
        branch.autosetupmerge = true;
        diff = {
          ignoreSubmodules = "dirty";
          renames = "copies";
          mnemonicprefix = true;
        };

        merge = {
          conflictstyle = "diff3";
          stat = true;
        };
        remote.origin.prune = true;
      };
    };

    gpg = { enable = true; };

    home-manager = { enable = true; };

    jq = { enable = true; };

    neovim = {
      enable = false;
      vimAlias = true;
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
    zsh = rec {
      enable = true;
      # seems have collision with nix.24
      enableCompletion = false;
      dotDir = ".config/zsh";
      sessionVariables = {
        PLANTUML_JAR_PATH = "${pkgs.plantuml}/lib/plantuml.jar";
        EUKLEIDES_PATH = "${pkgs.eukleides}/bin/eukleides";
        LANG = "en_US.UTF-8";
        GITSTATUS_LOG_LEVEL = "DEBUG";
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

          #THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
          export SDKMAN_DIR="$HOME/.sdkman"
          [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
        ''
      else
        lib.mkBefore "";

      oh-my-zsh = {
        enable = true;
        plugins = [ "history" "autojump" "history-substring-search" ];
        custom = "$HOME/.config/zsh/custom";
      };
    };
  };
}
