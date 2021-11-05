# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
{ config, lib, pkgs, localConfig, ... }:
let cfg = config.programs.editors.emacs;
in with lib; {
  options.programs.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    pkg = mkOption {
      type = types.package;
      default = pkgs.emacsMacport;
    };

    enableDoomConfig = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {

    home-manager.users.${localConfig.username} = {
      home.packages = with pkgs; [
        git
        (ripgrep.override { withPCRE2 = true; })
        gnutls # for TLS connectivity

        ## Optional dependencies
        fd # faster projectile indexing
        imagemagick # for image-dired
        zstd
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
      ];
      programs.emacs = {
        enable = true;
        package = cfg.pkg;
      };

      programs.zsh = {
        initExtra = ''
          export PATH=$PATH:$HOME/.emacs.d/bin
        '';
      };
    };

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # home-manager.users.${localConfig.username}.home.file =
    #   mkIf cfg.enableDoomConfig { ".doom.d".source = configDir + "/doom"; };
  };
}
