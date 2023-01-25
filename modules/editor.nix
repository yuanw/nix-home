# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
{ config, lib, pkgs, ... }:
let
  cfg = config.modules.editors.emacs;
  emacsclient = "${pkgs.emacs}/bin/emacsclient -c -a 'emacs'";

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
    services.emacs = {
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

        file = mkIf cfg.enableDoomConfig { ".doom.d".source = ../conf.d/doom; };
      };
      programs.emacs = {
        enable = true;
        package = cfg.pkg;
      };

      programs.zsh = {
        sessionVariables = { EDITOR = "${emacsclient}"; };
        initExtra = ''
          export PATH=$PATH:$HOME/.emacs.d/bin
        '';
      };
    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

  };
}
