{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

{
  imports = [
    inputs.catppuccin.homeManagerModules.catppuccin
  ];
  home.username = config.my.username;
  home.homeDirectory = config.my.homeDirectory;
  # https://rycee.gitlab.io/home-manager/release-notes.html#sec-release-22.11
  home.stateVersion = "22.11";
  home.packages =
    (import ./packages.nix { inherit pkgs; })
    ++ lib.optionals pkgs.stdenvNoCC.isDarwin (import ./macos_packages.nix { inherit pkgs; })
    ++ lib.optionals pkgs.stdenvNoCC.isLinux (import ./linux_packages.nix { inherit pkgs; });
  home.sessionPath = [
    "/usr/local/bin"
    "/usr/local/sbin"
    "${config.my.homeDirectory}/.local/bin"
  ];
  xdg = {
    enable = true;
    configFile = {
      # "startpage".source = ./startpage;
      "wallpapers/haskell-red-noise.png".source = ../pictures/haskell-red-noise.png;
      "wallpapers/nix-wallpaper-dracula.png".source = ../pictures/nix-wallpaper-dracula.png;
    };
  };
  catppuccin.flavor = "mocha";
  manual.manpages.enable = false;
  programs = {
    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [
        # batdiff
        batman
        batgrep
        batwatch
      ];
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
      nix-direnv = {
        enable = true;
      };
    };

    eza = {
      enable = true;
    };
    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    git = {
      enable = true;
      # package = pkgs.stable.git;
      userName = config.my.username;

      aliases = {
        co = "checkout";
        w = "status -sb";
        l =
          "log --graph --pretty=format:'%Cred%h%Creset"
          + " —%Cblue%d%Creset %s %Cgreen(%cr)%Creset'"
          + " --abbrev-commit --date=relative --show-notes=*";
      };
      userEmail = config.my.email;

      # difftastic = { enable = true; };
      # delta = { enable = true; };

      signing = {
        key = config.my.gpgKey;
        signByDefault = true;
      };
      ignores = [
        ".direnv"
        ".DS_Store"
        ".envrc"
        ".aider.chat.history.md"
        ".aider.input.history"
      ];
      # https://jvns.ca/blog/2024/02/16/popular-git-config-options/
      extraConfig = {
        core = {
          editor = "emacsclient -c";
        };
        init.defaultBranch = "main";
        pull.rebase = true;
        rebase.autosquash = true;
        push.default = "current";
        commit.verbose = true;
        help.autocorrect = 10;
        diff.algorithm = "histogram";
        branch.autosetupmerge = true;
        diff = {
          ignoreSubmodules = "dirty";
          renames = "copies";
          mnemonicprefix = true;
        };

        merge = {
          conflictstyle = "zdiff3";
          stat = true;
        };
        remote.origin.prune = true;
      };
    };

    gpg = {
      enable = true;
    };
    fastfetch = {
      enable = true;
    };

    home-manager = {
      enable = true;
    };

    jq = {
      enable = true;
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
    # https://github.com/nix-community/home-manager/blob/master/modules/programs/atuin.nix
    # https://docs.atuin.sh/configuration/config/
    atuin = {
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
        # EUKLEIDES_PATH = "${pkgs.eukleides}/bin/eukleides";
        LANG = "en_US.UTF-8";
        GITSTATUS_LOG_LEVEL = "DEBUG";
        HIST_STAMPS = "yyyy-mm-dd";
      };

      autosuggestion.enable = true;
      history = {
        size = 50000;
        save = 500000;
        path = "$HOME/.config/zsh/history";
        ignoreDups = true;
        share = true;
        ignoreSpace = true;
        extended = true;
        expireDuplicatesFirst = true;
        ignorePatterns = [
          "rm *"
          "pkill *"
          "ls"
        ];
      };

      initExtra =
        if pkgs.stdenvNoCC.isDarwin then
          lib.mkBefore ''
            setopt HIST_NO_STORE         # Don't store history commands
            setopt HIST_REDUCE_BLANKS    # Remove superfluous blanks from each command line being added to the history.i

          ''
        else
          lib.mkBefore ''
            setopt HIST_NO_STORE         # Don't store history commands
            setopt HIST_REDUCE_BLANKS    # Remove superfluous blanks from each command line being added to the history.
          '';

      oh-my-zsh = {
        enable = true;
        plugins = [
          "history"
          "autojump"
          "history-substring-search"
        ];
        custom = "$HOME/.config/zsh/custom";
      };
    };
  };
}
