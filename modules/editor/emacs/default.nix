# most of this is stealed from hlissner emacs module
# https://github.com/hlissner/dotfiles/blob/master/modules/editors/emacs.nix
# and adamcstephens emacs module
# https://github.com/adamcstephens/dotfiles/blob/34f28fc71cad6ffbf463eee00730f75ee39c1b4c/apps/emacs/default.nix
{ config, lib, pkgs, inputs, isDarwin, nurNoPkg, ... }:
let
  cfg = config.modules.editors.emacs;
  # inherit (pkgs) fetchurl fetchgit fetchFromGitHub stdenv lib;
  aspell = (pkgs.aspellWithDicts (ds: [ ds.en ds.en-computers ds.en-science ]));
  emacsclient = "emacsclient -c -a 'emacs'";
  emacsPatched = cfg.pkg.overrideAttrs (
    prev: {
      patches =
        (lib.optionals pkgs.stdenv.isDarwin [
          "${inputs.emacs-plus}/patches/emacs-30/fix-window-role.patch"
          "${inputs.emacs-plus}/patches/emacs-30/poll.patch"
          "${inputs.emacs-plus}/patches/emacs-30/round-undecorated-frame.patch"
          "${inputs.emacs-plus}/patches/emacs-30/system-appearance.patch"
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
        flycheck
        flycheck-eglot
        eglot
        eglot-tempel
        free-keys
        keycast
        git-link
        google-this
        groovy-mode
        goto-last-change
        gptel
        graphviz-dot-mode
        haskell-mode
        (callPackage ./transient-showcase.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild transient;
        })
        (callPackage ./auto-save.nix {
          inherit (pkgs) fetchFromGitHub;
          inherit (epkgs) trivialBuild;
        })
        treesit-grammars.with-all-grammars
        (epkgs.tree-sitter-langs.withPlugins (_p: epkgs.tree-sitter-langs.plugins ++ [
          _p.tree-sitter-markdown
        ]))
        (
          callPackage ./lsp-bridge.nix {
            inherit (pkgs) fetchFromGitHub substituteAll writeText python3;
            inherit (epkgs) melpaBuild markdown-mode yasnippet;
          }
        )
        denote
        doom-modeline
        doom-themes
        emacsql
        emacsql-sqlite
        embark
        embark-consult
        exec-path-from-shell
        helpful
        hydra
        jinx
        just-mode
        justl

        kind-icon
        magit
        marginalia
        markdown-mode
        meow
        multi-vterm
        nerd-icons
        nix-mode
        orderless
        org
        org-roam
        smartparens
        super-save
        telephone-line
        tree-sitter
        tsc
        tmr
        use-package
        vertico
        vterm
        vterm-toggle
        which-key
        yaml-mode
        yaml-mode
        zoom
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
      default = pkgs.emacs-git;
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

  config = mkIf cfg.enable (mkMerge [
    {

      # https://www.reddit.com/r/NixOS/comments/vh2kf7/home_manager_mkoutofstoresymlink_issues/
      # config.lib.file.mkOutOfStoreSymlink is provided by the home-manager module,
      # but it appears { config, pkgs, ...}: at the top of users/nic/default.nix is not running in
      # the context of home-manager
      home-manager.users.${config.my.username} = { pkgs, ... }:
        {
          imports = [
            nurNoPkg.repos.rycee.hmModules.emacs-init
          ];
          programs.emacs.extraPackages = epkgs:
            with epkgs;
            [ epkgs.treesit-grammars.with-all-grammars ];
          programs.emacs.package = emacsPatched;
          programs.emacs.enable = true;
          programs.emacs.init = {
            enable = true;
            packageQuickstart = false;
            recommendedGcSettings = true;
            usePackageVerbose = false;

            earlyInit = ''
              ;; Disable some GUI distractions. We set these manually to avoid starting
              ;; the corresponding minor modes.
              (push '(menu-bar-lines . 0) default-frame-alist)
              (push '(tool-bar-lines . nil) default-frame-alist)
              (push '(vertical-scroll-bars . nil) default-frame-alist)

              ;; Set up fonts early.
              (set-face-attribute 'default nil
              :font "PragmataPro Mono Liga"
              :height 180
              :weight 'medium)


            '';

            prelude = ''
              ;; Disable startup message.
              (setq inhibit-startup-screen t
                    inhibit-startup-echo-area-message (user-login-name))

              (setq initial-major-mode 'fundamental-mode
                    initial-scratch-message nil)

              ;; Don't blink the cursor.
              (setq blink-cursor-mode nil)

              ;; Set frame title.
              (setq frame-title-format
                    '("" invocation-name ": "(:eval
                                              (if (buffer-file-name)
                                                  (abbreviate-file-name (buffer-file-name))
                                                "%b"))))

              ;; Make sure the mouse cursor is visible at all times.
              (set-face-background 'mouse "#ffffff")

              ;; Accept 'y' and 'n' rather than 'yes' and 'no'.
              (defalias 'yes-or-no-p 'y-or-n-p)

              ;; Don't want to move based on visual line.
              (setq line-move-visual nil)

              ;; Stop creating backup and autosave files.
              (setq make-backup-files nil
                    auto-save-default nil)

              ;; Default is 4k, which is too low for LSP.
              (setq read-process-output-max (* 1024 1024))

              ;; Always show line and column number in the mode line.
              (line-number-mode)
              (column-number-mode)

              ;; Enable CUA mode.
              (cua-mode 1)

              ;; Enable some features that are disabled by default.
              (put 'narrow-to-region 'disabled nil)

              ;; Typically, I only want spaces when pressing the TAB key. I also
              ;; want 4 of them.
              (setq-default indent-tabs-mode nil
                            tab-width 4
                            c-basic-offset 4)

              ;; Trailing white space are banned!
              (setq-default show-trailing-whitespace t)

              ;; Use one space to end sentences.
              (setq sentence-end-double-space nil)

              ;; I typically want to use UTF-8.
              (prefer-coding-system 'utf-8)

              ;; Nicer handling of regions.
              (transient-mark-mode 1)

              ;; Make moving cursor past bottom only scroll a single line rather
              ;; than half a page.
              (setq scroll-step 1
                    scroll-conservatively 5)

              ;; Enable highlighting of current line.
              (global-hl-line-mode 1)

              ;; Avoid noisy bell.
              (setq visible-bell t)

              ;; Improved handling of clipboard in GNU/Linux and otherwise.
              (setq select-enable-clipboard t
                    select-enable-primary t
                    save-interprogram-paste-before-kill t)

              ;; Pasting with middle click should insert at point, not where the
              ;; click happened.
              (setq mouse-yank-at-point t)

              ;; Only do candidate cycling if there are very few candidates.
              (setq completion-cycle-threshold 3)

              ;; Enable a few useful commands that are initially disabled.
              (put 'upcase-region 'disabled nil)
              (put 'downcase-region 'disabled nil)

              ;;(setq custom-file (locate-user-emacs-file "custom.el"))
              ;;(load custom-file)

              ;; When finding file in non-existing directory, offer to create the
              ;; parent directory.
              (defun with-buffer-name-prompt-and-make-subdirs ()
                (let ((parent-directory (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p parent-directory))
                             (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
                    (make-directory parent-directory t))))

              (add-to-list 'find-file-not-found-functions #'with-buffer-name-prompt-and-make-subdirs)

              ;; Don't want to complete .hi files.
              (add-to-list 'completion-ignored-extensions ".hi")

              ;; Try out some tree-sitter modes.
              (setq major-mode-remap-alist
               '((bash-mode . bash-ts-mode)))

              (defun rah-disable-trailing-whitespace-mode ()
                (setq show-trailing-whitespace nil))

              ;; Shouldn't highlight trailing spaces in terminal mode.
              (add-hook 'term-mode #'rah-disable-trailing-whitespace-mode)
              (add-hook 'term-mode-hook #'rah-disable-trailing-whitespace-mode)

              ;; Ignore trailing white space in compilation mode.
              (add-hook 'compilation-mode-hook #'rah-disable-trailing-whitespace-mode)

              (defun rah-prog-mode-setup ()
                ;; Use a bit wider fill column width in programming modes
                ;; since we often work with indentation to start with.
                (setq fill-column 80))

              (add-hook 'prog-mode-hook #'rah-prog-mode-setup)

              (defun rah-lsp ()
                (interactive)
                (envrc-mode)
                (lsp))

              (defun rah-insert-uuid-v4 ()
                (interactive)
                (let ((lines (process-lines "uuidgen" "--random")))
                  (insert (car lines))))

              (defun rah-insert-timestamp ()
                (interactive)
                (let ((lines (process-lines "date" "--iso-8601=second" "--universal")))
                  (insert (car lines))))

              ;(defun rah-sort-lines-ignore-case ()
              ;  (interactive)
              ;  (let ((sort-fold-case t))
              ;    (call-interactively 'sort-lines)))
            '';

            postlude = ''
              ;(unbind-key "C-a")
              ;(unbind-key "C-e")

              ;(bind-keys ("C-o" . find-file)
              ;           ("C-<right>" . forward-word)
              ;           ("C-<left>" . backward-word))
            '';

            usePackage = {
              meow = {
                enable = true;
                demand = true;
                init = ''
                        (defun meow-setup ()
                      (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
                       (meow-motion-overwrite-define-key
                        '("j" . meow-next)
                        '("k" . meow-prev)
                        '("<escape>" . ignore))
                      (meow-leader-define-key
                       ;; SPC j/k will run the original command in MOTION state.
                       '("j" . "H-j")
                       '("k" . "H-k")
                       ;; Use SPC (0-9) for digit arguments.
                       '("1" . meow-digit-argument)
                       '("2" . meow-digit-argument)
                       '("3" . meow-digit-argument)
                       '("4" . meow-digit-argument)
                       '("5" . meow-digit-argument)
                       '("6" . meow-digit-argument)
                       '("7" . meow-digit-argument)
                       '("8" . meow-digit-argument)
                       '("9" . meow-digit-argument)
                       '("0" . meow-digit-argument)
                       '("/" . meow-keypad-describe-key)
                   )
                      (meow-normal-define-key
                       '("0" . meow-expand-0)
                       '("9" . meow-expand-9)
                       '("8" . meow-expand-8)
                       '("7" . meow-expand-7)
                       '("6" . meow-expand-6)
                       '("5" . meow-expand-5)
                       '("4" . meow-expand-4)
                       '("3" . meow-expand-3)
                       '("2" . meow-expand-2)
                       '("1" . meow-expand-1)
                       '("-" . negative-argument)
                       '(";" . meow-reverse)
                       '("," . meow-inner-of-thing)
                       '("." . meow-bounds-of-thing)
                       '("[" . meow-beginning-of-thing)
                       '("]" . meow-end-of-thing)
                       '("a" . meow-append)
                       '("A" . meow-open-below)
                       '("b" . meow-back-word)
                       '("B" . meow-back-symbol)
                       '("c" . meow-change)
                       '("d" . meow-delete)
                       '("D" . meow-backward-delete)
                       '("e" . meow-next-word)
                       '("E" . meow-next-symbol)
                       '("f" . meow-find)
                       '("g" . meow-cancel-selection)
                       '("G" . meow-grab)
                       '("h" . meow-left)
                       '("H" . meow-left-expand)
                       '("i" . meow-insert)
                       '("I" . meow-open-above)
                       '("j" . meow-next)
                       '("J" . meow-next-expand)
                       '("k" . meow-prev)
                       '("K" . meow-prev-expand)
                       '("l" . meow-right)
                       '("L" . meow-right-expand)
                       '("m" . meow-join)
                       '("n" . meow-search)
                       '("o" . meow-block)
                       '("O" . meow-to-block)
                       '("p" . meow-yank)
                       '("q" . meow-quit)
                       '("Q" . meow-goto-line)
                       '("r" . meow-replace)
                       '("R" . meow-swap-grab)
                       '("s" . meow-kill)
                       '("t" . meow-till)
                       '("u" . meow-undo)
                       '("U" . meow-undo-in-selection)
                       '("v" . meow-visit)
                       '("w" . meow-mark-word)
                       '("W" . meow-mark-symbol)
                       '("x" . meow-line)
                       '("X" . meow-goto-line)
                       '("y" . meow-save)
                       '("Y" . meow-sync-grab)
                       '("z" . meow-pop-selection)
                       '("'" . repeat)
                       '("<escape>" . ignore)))
     
    
                      (defun meow-clipboard-toggle ()
                        (interactive)
                  (if meow-use-clipboard
                      (progn
                        (setq meow-use-clipboard nil)
                        (message "Meow clipboard usage disabled"))
                    (progn
                      (setq meow-use-clipboard t)
                      (message "Meow clipboard usage enabled"))))
                '';
                config = ''
                  (setq meow-use-clipboard t)
                  (meow-setup)
                  (meow-global-mode 1)'';

              };

              autorevert = {
                hook = "(dired-mode . auto-revert-mode)";
                custom = "(auto-revert-use-notify nil)";
                config = "(global-auto-revert-mode t)";
              };
            };



          };

          # xdg.configFile."emacs".source = emacsConfigPath;

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
              tree-sitter
              # emacsWithDeps
              vale
            ];
            # file.".emacs.d".source = emacsConfigPath;
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

          };
          # not use home-manager programs.emacs due to it wraps
          # emacsWithPackages again
          programs.zsh = {
            sessionVariables = {
              EDITOR = "${emacsclient}";
              EMACS_DIR = "${emacsWithDeps}";
              ASPELL_CONF = "dict-dir ${aspell}/lib/aspell";
            };

          };
        };



    }
    (if (isDarwin) then {
      fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
    } else {
      fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];
    })

    (if (builtins.hasAttr "launchd" options) then {
      services.emacs = mkIf cfg.usePackage {
        enable = cfg.enableService;
        package = config.programs.emacs.finalPackage;
      };
      launchd.user.agents.emacs.serviceConfig = {
        StandardOutPath = "/tmp/emacs.log";
        StandardErrorPath = "/tmp/emacs.log";
      };
    } else
      { })
  ]);
}
