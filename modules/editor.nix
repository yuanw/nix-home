# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
{ config, lib, pkgs, ... }:
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

  inherit (config.lib.file) mkOutOfStoreSymlink;
in with lib; {
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

    home-manager.users.${config.my.username} = {
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
          ".doom.d/init.el".source =
            mkOutOfStoreSymlink ../conf.d/doom-work/init.el;
          ".doom.d/packages.el".source =
            mkOutOfStoreSymlink ../conf.d/doom-work/packages.el;
          ".doom.d/config.el".source =
            mkOutOfStoreSymlink ../conf.d/doom-work/config.el;
        };
      };
      programs.emacs = mkIf cfg.usePackage {
        enable = true;
        package = cfg.pkg;
      };

      programs.zsh = {
        sessionVariables = { EDITOR = "${emacsclient}"; };
        initExtra = ''
          export PATH=$PATH:$HOME/.emacs.d/bin
          export PATH=$PATH:$HOME/.config/emacs/bin
        '';
      };
    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

  };
}
