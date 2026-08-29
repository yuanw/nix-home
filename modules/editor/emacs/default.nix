# most of this is stealed from rycee emacs module
# https://git.sr.ht/~rycee/configurations/tree/master/item/user/emacs.nix
# other codes stealed from
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
# and adamcstephens emacs module
# https://github.com/adamcstephens/dotfiles/blob/34f28fc71cad6ffbf463eee00730f75ee39c1b4c/apps/emacs/default.nix
{
  config,
  lib,
  pkgs,
  isDarwin,
  ...
}:
let
  cfg = config.modules.editors.emacs;
  aspell = (
    pkgs.aspellWithDicts (ds: [
      ds.en
      ds.en-computers
      ds.en-science
    ])
  );
  emacsclient = "${emacsPackage}/bin/emacsclient -c -a '${emacsPackage}/bin/emacs'";
  emacsPackage = import ./nima.nix {
    inherit pkgs lib;
    myConfig = config.my;
    emacsConfig = cfg;
  };
in
with lib;
{
  options.modules.editors.emacs = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    pkg = mkOption {
      type = types.package;
      default = pkgs.emacs-git;
    };

    lspStyle = mkOption {
      type = types.enum [
        "eglot"
        "lsp-bridge"
        "lspce"
        "lsp-mode"
      ];
      default = "eglot";
    };
    enableLatex = mkOption {
      type = types.bool;
      default = true;
    };

    enableService = mkOption {
      type = types.bool;
      default = false;
    };

    enableAider = mkOption {
      type = types.bool;
      default = false;
    };
    enableCopilot = mkOption {
      type = types.bool;
      default = false;
    };

    modalEditing = mkOption {
      type = types.enum [
        "none"
        "meow"
        "hel"
      ];
      default = "meow";
      description = ''
        Modal editing layer for Emacs.
        - "none": no modal editing
        - "meow": Meow modal editor
        - "hel": Helix emulation layer
      '';
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (!isDarwin) {
      services.emacs = {
        enable = cfg.enableService;
        package = emacsPackage;
      };
    })

    (mkIf cfg.enableLatex {
      home-manager.users.${config.my.username} =
        { pkgs, ... }:
        {
          home.packages = with pkgs; [
            stable.texlive.combined.scheme-medium
          ];
        };
    })

    {
      fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];
      # https://www.reddit.com/r/NixOS/comments/vh2kf7/home_manager_mkoutofstoresymlink_issues/
      # config.lib.file.mkOutOfStoreSymlink is provided by the home-manager module,
      # but it appears { config, pkgs, ...}: at the top of users/nic/default.nix is not running in
      # the context of home-manager
      home-manager.users.${config.my.username} =
        hm@{ pkgs, ... }:
        {
          # Emacs is provided by `./nima.nix` and installed directly via
          # `home.packages` below.  Do not enable Home Manager's Emacs
          # wrapper/init generator, otherwise Emacs would be wrapped twice.
          programs.emacs.enable = false;

          home = {
            file.".emacs.d/early-init.el".text = import ./early-init.nix {
              monoFont = config.my.monoFont;
              font = config.my.font;
              inherit isDarwin;
            };

            file.".emacs.d/snippets".source =
              hm.config.lib.file.mkOutOfStoreSymlink "${config.my.homeDirectory}/${config.my.workspaceDirectory}/nix-home/modules/editor/emacs/snippets";
            packages = with pkgs; [
              emacsPackage
              (pkgs.writeShellScriptBin "app-launcher" ''
                ${emacsPackage}/bin/emacsclient --eval "(consult-omni-app-launcher)"
              '')
              (pkgs.writeShellScriptBin "org-capture" ''
                ${emacsPackage}/bin/emacsclient -n -e '(yequake-toggle "org-capture")'
              '')
              # git
              (ripgrep.override { withPCRE2 = true; })
              gnutls # for TLS connectivity
              cmake
              #enchant
              ## Optional dependencies
              fd # faster file finding for project.el
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
              #: js
              # nodePackages.eslint
              #: markdown

              #: sh
              bash-language-server
              #: toml
              taplo
              #: web-mode
              js-beautify
              stylelint
              # :lang yaml
              yaml-language-server
              tree-sitter
              # emacsWithDeps
              (vale.withStyles (s: [
                s.alex
                s.google
                s.microsoft
                s.joblint
                s.proselint
                s.write-good
              ]))

            ];
            file.".vale.ini".text = ''
              [*]
              BasedOnStyles = alex, Google, Microsoft, Joblint, proselint, write-good
            '';
            # file.".emacs.d".source = emacsConfigPath;
          };
          programs.zsh = {
            sessionVariables = {
              EDITOR = "${emacsclient}";
              ASPELL_CONF = "dict-dir ${aspell}/lib/aspell";
            };

          };
        };
    }
  ]);
}
