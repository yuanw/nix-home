# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
# and adamcstephens emacs module
# https://github.com/adamcstephens/dotfiles/blob/34f28fc71cad6ffbf463eee00730f75ee39c1b4c/apps/emacs/default.nix
{ config, lib, pkgs, inputs, isDarwin, ... }:
let
  cfg = config.modules.editors.emacs;
  emacsclient = "emacsclient -c -a 'emacs'";
  # https://gist.github.com/hlissner/ba8c3b4c6f37c24ff27b72194942b7aa
  emacsPatched = cfg.package.overrideAttrs (
    prev: {
      patches =
        (lib.optionals pkgs.stdenv.isDarwin [
          "${inputs.emacs-plus}/patches/emacs-28/fix-window-role.patch"
          "${inputs.emacs-plus}/patches/emacs-28/no-frame-refocus-cocoa.patch"
          "${inputs.emacs-plus}/patches/emacs-29/poll.patch"
          # "${inputs.emacs-plus}/patches/emacs-30/round-undecorated-frame.patch"
          "${inputs.emacs-plus}/patches/emacs-28/system-appearance.patch"
        ])
        ++ prev.patches;
    }
  );
  emacsWithDeps =
    (pkgs.emacsPackagesFor (emacsPatched)).emacsWithPackages (epkgs:
      with epkgs;
      # Use Nix to manage packages with non-trivial userspace dependencies.
      [
        emacsql
        emacsql-sqlite

        # FIXME: Currently building `epdinfo` on macOS like so:
        # ; git clone https://github.com/vedang/pdf-tools.git
        # ; cd pdf-tools/server
        # ; nix-shell -p pkg-config poppler automake libtool libpng autoconf
        # ; autoreconf -i -f
        # ; ./autobuild -i \
        #   /Users/mbaillie/.config/emacs/.local/straight/build-29.0.50/pdf-tools \
        #   --os nixos
        # org-pdftools
        vterm
      ]
    );

in
with lib; {

  options.modules.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    pkg = mkOption {
      type = types.package;
      default = pkgs.emacs29;
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
      package = emacsWithDeps;
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
        package = emacsWithDeps;
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
      fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
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
