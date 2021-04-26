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
        # https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/tmux#configuration-variables
        # automatically start tmux
        ZSH_TMUX_AUTOSTART = "true";
        ZSH_TMUX_CONFIG = "${config.xdg.dataHome}/tmux/tmux.conf";
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
