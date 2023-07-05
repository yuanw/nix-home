# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
{ config, lib, pkgs, doom-emacs, ... }:
let
  cfg = config.modules.editors.emacs;
  emacsclient = "emacsclient -c -a 'emacs'";
  # https://gist.github.com/hlissner/ba8c3b4c6f37c24ff27b72194942b7aa
  writeDoomScript = name: text:
    pkgs.writeTextFile {
      inherit name;
      executable = true;
      text = ''
        #!/usr/bin/env doomscript
        ${text}
      '';
      checkPhase = ''
        ${lib.stdenv.shellDryRun} "$target"
      '';
    };

in
with lib; {

  options.modules.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    pkg = mkOption {
      type = types.package;
      default = pkgs.emacsMacport;
    };

    usePackage = mkOption {
      type = types.bool;
      default = true;
    };

    enableService = mkOption {
      type = types.bool;
      default = false;
    };

    enableDoomConfig = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.emacs = mkIf cfg.usePackage {
      enable = cfg.enableService;
      package = cfg.pkg;
    };
    # https://www.reddit.com/r/NixOS/comments/vh2kf7/home_manager_mkoutofstoresymlink_issues/
    # config.lib.file.mkOutOfStoreSymlink is provided by the home-manager module,
    # but it appears { config, pkgs, ...}: at the top of users/nic/default.nix is not running in
    # the context of home-manager
    home-manager.users.${config.my.username} = { config, pkgs, ... }: {
      home = {
        packages = with pkgs; [
          # git
          (ripgrep.override { withPCRE2 = true; })
          gnutls # for TLS connectivity
          cmake
          ## Optional dependencies
          fd # faster projectile indexing
          imagemagick # for image-dired
          zstd
          html-tidy
          shfmt
          ## Module dependencies
          # :checkers spell
          (aspellWithDicts (ds: [ ds.en ds.en-computers ds.en-science ]))
          # :checkers grammar
          languagetool
          # :tools editorconfig
          editorconfig-core-c # per-project style config
          # :tools lookup & :lang org +roam
          sqlite
          tree-sitter
          (tree-sitter.withPlugins (p: [ p.tree-sitter-c p.tree-sitter-java ]))
          wordnet
          # :lang latex & :lang org (latex previews)
          texlive.combined.scheme-medium
          #: js
          nodePackages.eslint
          #: markdown
          nodePackages.unified-language-server
          #: sh
          nodePackages.bash-language-server
          #: toml
          taplo-lsp
          #: web-mode
          nodePackages.js-beautify
          nodePackages.stylelint
          # :lang yaml
          nodePackages.yaml-language-server
        ];

        file = mkIf cfg.enableDoomConfig {
          ".doom.d".source = ../conf.d/doom;
        };
      };

      programs.emacs = mkIf cfg.usePackage {
        enable = true;
        package = cfg.pkg;
      };
      # https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix#L58
      #
      programs.zsh = {
        sessionVariables = { EDITOR = "${emacsclient}"; };
        initExtra = ''
          export PATH=$PATH:$XDG_CONFIG_HOME/emacs/bin
        '';
      };
    };


    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    system.userActivationScripts = {
      installDoomEmacs = ''
        if [ ! -d "$XDG_CONFIG_HOME/emacs" ]; then
           git clone --depth=1 --single-branch "https://github.com/doomemacs/doomemacs" "$XDG_CONFIG_HOME/emacs"
        fi
      '';
    };

  };
}
