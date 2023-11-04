# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
{ config, lib, pkgs, isDarwin, ... }:
let
  cfg = config.modules.editors.emacs;
  emacsclient = "emacsclient -c -a 'emacs'";
  # https://gist.github.com/hlissner/ba8c3b4c6f37c24ff27b72194942b7aa

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

  config = mkIf cfg.enable (mkMerge [{
    services.emacs = mkIf cfg.usePackage {
      enable = cfg.enableService;
      package = cfg.pkg;
    };
    # https://www.reddit.com/r/NixOS/comments/vh2kf7/home_manager_mkoutofstoresymlink_issues/
    # config.lib.file.mkOutOfStoreSymlink is provided by the home-manager module,
    # but it appears { config, pkgs, ...}: at the top of users/nic/default.nix is not running in
    # the context of home-manager
    home-manager.users.${config.my.username} = { pkgs, ... }: {
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
          ".doom.d".source = ./config;
        };
      };

      programs.emacs = mkIf cfg.usePackage {
        enable = true;
        package = cfg.pkg;
      };
      programs.zsh = {
        sessionVariables = { EDITOR = "${emacsclient}"; };
        initExtra = ''
          export PATH=$PATH:$XDG_CONFIG_HOME/emacs/bin
          export PATH=$PATH:$HOME/.doom.d/bin
        '';
      };
    };



  }
    (if (isDarwin) then {
      fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];
    } else {

      fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];
    })
    (if (builtins.hasAttr "launchd" options) then {

      launchd.user.agents.emacs.serviceConfig = {
        StandardOutPath = "/tmp/emacs.log";
        StandardErrorPath = "/tmp/emacs.log";
      };
    } else
      {

        # systemd
      })]);
}
