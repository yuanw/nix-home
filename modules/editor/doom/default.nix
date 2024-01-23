# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
# and adamcstephens emacs module
# https://github.com/adamcstephens/dotfiles/blob/34f28fc71cad6ffbf463eee00730f75ee39c1b4c/apps/emacs/default.nix
{ config, lib, pkgs, inputs, isDarwin, ... }:
let
  cfg = config.modules.editors.emacs;

  aspell = (pkgs.aspellWithDicts (ds: [ ds.en ds.en-computers ds.en-science ]));
  emacsclient = "emacsclient -c -a 'emacs'";
  # https://gist.github.com/hlissner/ba8c3b4c6f37c24ff27b72194942b7aa
  emacsPatched = cfg.pkg.overrideAttrs (
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
        ace-window
        aggressive-indent
        avy
        cape
        corfu
        consult
        consult-dir
        consult-eglot
        consult-flycheck
        consult-git-log-grep
        consult-yasnippet
        consult-org-roam
        direnv
        eglot-tempel
        free-keys
        keycast
        google-this
        goto-last-change
        gptel
        graphviz-dot-mode
        haskell-mode
        hydra
        doom-themes
        doom-modeline
        emacsql
        emacsql-sqlite
        embark
        exec-path-from-shell
        embark-consult
        jinx
        justl
        just-mode
        magit
        marginalia
        markdown-mode
        meow
        nix-mode
        nerd-icons
        telephone-line
        orderless
        org
        org-roam
        yaml-mode
        tree-sitter
        (epkgs.tree-sitter-langs.withPlugins (p: epkgs.tree-sitter-langs.plugins ++ [
          p.tree-sitter-markdown
        ]))
        vertico
        vterm
        vterm-toggle
        multi-vterm
        which-key
        zoom
        yaml-mode
      ]
    );
  valeStyles = [
    { name = "alex"; path = "${inputs.vale-alex}/alex"; }
    { name = "Google"; path = "${inputs.vale-Google}/Google"; }
    { name = "Microsoft"; path = "${inputs.vale-Microsoft}/Microsoft"; }
    { name = "Joblint"; path = "${inputs.vale-Joblint}/Joblint"; }
    { name = "proselint"; path = "${inputs.vale-proselint}/proselint"; }
    { name = "write-good"; path = "${inputs.vale-write-good}/write-good"; }
  ];
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
    home-manager.users.${config.my.username} = { pkgs, config, ... }:
      let mkLink = config.lib.file.mkOutOfStoreSymlink; in
      {

        xdg.configFile."doom-profiles.el".source = ./profiles.el;
        xdg.configFile."emacs".source = mkLink
          "${config.home.homeDirectory}/workspaces/nix-home/modules/editor/doom/zero";
        home = {
          packages = with pkgs; [
            # git
            (ripgrep.override { withPCRE2 = true; })
            gnutls # for TLS connectivity
            cmake
            enchant
            ## Optional dependencies
            fd # faster projectile indexing
            imagemagick # for image-dired
            zstd
            html-tidy
            shfmt
            ## Module dependencies
            # :checkers spell
            aspell
            # :checkers grammar
            languagetool
            # :tools editorconfig
            editorconfig-core-c # per-project style config
            # :tools lookup & :lang org +roam
            sqlite
            # wordnet
            # :lang latex & :lang org (latex previews)
            texlive.combined.scheme-medium
            #: js
            # nodePackages.eslint
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
            emacsWithDeps
            vale
          ];

          file.".vale.ini".text =
            let
              stylesPath = pkgs.linkFarm "vale-styles" valeStyles;
              basedOnStyles = concatStringsSep ", "
                (zipAttrsWithNames [ "name" ] (_: v: v) valeStyles).name;
            in
            ''
              StylesPath = ${stylesPath}
              [*]
              BasedOnStyles = ${basedOnStyles}
            '';

          file.".doom.d".source = ./config;
        };
        # not use home-manager programs.emacs due to it wraps
        # emacsWithPackages again
        programs.zsh = {
          sessionVariables = {
            EDITOR = "${emacsclient}";
            ASPELL_CONF = "dict-dir ${aspell}/lib/aspell";
          };
          initExtra = ''
            export PATH=$PATH:$XDG_CONFIG_HOME/emacs/bin
            export PATH=$PATH:$HOME/.doom.d/bin
          '';
        };
      };



  }
    (if (isDarwin) then {
      # fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
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
