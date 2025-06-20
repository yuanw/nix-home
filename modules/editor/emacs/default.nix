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
  inputs,
  isDarwin,
  ...
}:
let
  cfg = config.modules.editors.emacs;
  # inherit (pkgs) fetchurl fetchgit fetchFromGitHub stdenv lib;
  aspell = (
    pkgs.aspellWithDicts (ds: [
      ds.en
      ds.en-computers
      ds.en-science
    ])
  );
  emacsclient = "emacsclient -c -a 'emacs'";
  emacsPatched = cfg.pkg.overrideAttrs (prev: {
    patches =
      (lib.optionals pkgs.stdenv.isDarwin [
        "${inputs.emacs-plus}/patches/emacs-31/fix-window-role.patch"
        "${inputs.emacs-plus}/patches/emacs-31/round-undecorated-frame.patch"
        "${inputs.emacs-plus}/patches/emacs-31/system-appearance.patch"
      ])
      ++ prev.patches;
  });

  emacsPackage = config.home-manager.users.${config.my.username}.programs.emacs.finalPackage;

  pkgs' = pkgs.extend (
    _final: prev: {
      ld64 = prev.ld64.overrideAttrs (old: {
        patches = old.patches ++ [ ./Dedupe-RPATH-entries.patch ];
      });
      libarchive = prev.libarchive.overrideAttrs (_old: {
        doCheck = false;
      });
    }
  );
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
      # https://github.com/NixOS/nixpkgs/issues/395169
      default =
        if isDarwin then pkgs'.emacs-git.override { withNativeCompilation = true; } else pkgs.emacs-git;
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
          imports = [
            ./emacs-init.nix
          ];

          programs.emacs.extraPackages =
            epkgs: with epkgs; [
              epkgs.treesit-grammars.with-all-grammars
              (epkgs.trivialBuild {
                pname = "prot-common";
                version = "0.0.1";
                src = ./packages/prot-common.el;
              })
            ];
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
                            ;; no title bar
                            (add-to-list 'default-frame-alist '(undecorated-round . t))
                            ;; Set up fonts early.
                            ;;--------------------
                            (let ((mono-spaced-font "${config.my.monoFont}")
                             (proportionately-spaced-font "${config.my.font}"))
                             (set-face-attribute 'default nil :family mono-spaced-font :height 180)
                             (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
                             (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

                            ;; auto-save might handle this already 
                            (setq make-backup-files nil)
                            (setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
                            (setq create-lockfiles nil)

                            ;; Make native compilation silent and prune its cache.
                            (when (native-comp-available-p)
                              (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
                              (setq native-compile-prune-cache t)) ; Emacs 29

                            ;; Luminosity 50
                            (defconst palette-red               "#FF0000")
                            (defconst palette-orange            "#FF8000")
                            (defconst palette-yellow            "#FFFF00")
                            (defconst palette-yellow-green      "#80FF00")
                            (defconst palette-green             "#00FF00")
                            (defconst palette-teal              "#00FF80")
                            (defconst palette-cyan              "#00FFFF")
                            (defconst palette-slate-blue        "#007FFF")
                            (defconst palette-blue              "#0000FF")
                            (defconst palette-indigo            "#7F00FF")
                            (defconst palette-purple            "#FF00FF")
                            (defconst palette-fuschia           "#FF0080")

                            ;; Luminosity 20
                            (defconst palette-red-dark          "#660000")
                            (defconst palette-orange-dark       "#663300")
                            (defconst palette-yellow-dark       "#666600")
                            (defconst palette-yellow-green-dark "#336600")
                            (defconst palette-green-dark        "#006600")
                            (defconst palette-teal-dark         "#006633")
                            (defconst palette-cyan-dark         "#006666")
                            (defconst palette-slate-blue-dark   "#003366")
                            (defconst palette-blue-dark         "#000066")
                            (defconst palette-indigo-dark       "#330066")
                            (defconst palette-purple-dark       "#660066")
                            (defconst palette-fuschia-dark      "#660033")

                            ;; Luminosity 15
                            (defconst palette-red-darker          "#4D0000")
                            (defconst palette-orange-darker       "#4D2600")
                            (defconst palette-yellow-darker       "#4D4D00")
                            (defconst palette-yellow-green-darker "#264D00")
                            (defconst palette-green-darker        "#004D00")
                            (defconst palette-teal-darker         "#004D26")
                            (defconst palette-cyan-darker         "#004D4D")
                            (defconst palette-slate-blue-darker   "#00264D")
                            (defconst palette-blue-darker         "#00004D")
                            (defconst palette-indigo-darker       "#26004D")
                            (defconst palette-purple-darker       "#4D004D")
                            (defconst palette-fuschia-darker      "#4D0026")

                            ;; Luminosity 10
                            (defconst palette-red-darkest          "#330000")
                            (defconst palette-orange-darkest       "#331A00")
                            (defconst palette-yellow-darkest       "#333300")
                            (defconst palette-yellow-green-darkest "#1A3300")
                            (defconst palette-green-darkest        "#003300")
                            (defconst palette-teal-darkest         "#00331A")
                            (defconst palette-cyan-darkest         "#003333")
                            (defconst palette-slate-blue-darkest   "#001A33")
                            (defconst palentte-blue-darkest         "#000033")
                            (defconst palette-indigo-darkest       "#1A0033")
                            (defconst palette-purple-darkest       "#330033")
                            (defconst palette-fuschia-darkest      "#33001A")

                            ;; Make customisations that affect Emacs faces BEFORE loading a theme
              ;; (any change needs a theme re-load to take effect).
              (require 'ef-themes)

              ;; If you like two specific themes and want to switch between them, you
              ;; can specify them in `ef-themes-to-toggle' and then invoke the command
              ;; `ef-themes-toggle'.  All the themes are included in the variable
              ;; `ef-themes-collection'.
              (setq ef-themes-to-toggle '(ef-day ef-winter))

              (setq ef-themes-headings ; read the manual's entry or the doc string
                    '((0 variable-pitch light 1.9)
                      (1 variable-pitch light 1.8)
                      (2 variable-pitch regular 1.7)
                      (3 variable-pitch regular 1.6)
                      (4 variable-pitch regular 1.5)
                      (5 variable-pitch 1.4) ; absence of weight means `bold'
                      (6 variable-pitch 1.3)
                      (7 variable-pitch 1.2)
                      (t variable-pitch 1.1)))

              ;; They are nil by default...
              (setq ef-themes-mixed-fonts t
                    ef-themes-variable-pitch-ui t)

              ;; Disable all other themes to avoid awkward blending:
              (mapc #'disable-theme custom-enabled-themes)


              ;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
              (ef-themes-select 'ef-day)
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
                          ;; (defalias 'yes-or-no-p 'y-or-n-p)
                          (setopt use-short-answers t)
                          (setopt use-dialog-box nil)

                          ;; Don't want to move based on visual line.
                          (setq line-move-visual nil)

                             ;; TODO maybe should re-configure this
                             ;; Stop creating backup and autosave files.
                             (setopt make-backup-files nil
                                   auto-save-default nil)
                                  
                                  
                             ;; Default is 4k, which is too low for LSP.
                             (setq read-process-output-max (* 1024 1024))

                             ;; Always show line and column number in the mode line.
                             (line-number-mode)
                             (column-number-mode)
                             (global-visual-line-mode t)
                             
                             ;; Enable some features that are disabled by default.
                             (dolist (cmd '(narrow-to-region
                                            upcase-region
                                            downcase-region
                                            dired-find-alternate-file
                                            LaTeX-narrow-to-environment
                                            TeX-narrow-to-group
                                            narrow-to-page
                                            set-goal-column
                                            scroll-left
                                            scroll-right))
                                            (put cmd 'disabled nil))
                             ;; Minimising & quitting Emacs way too many times without wanting to.
                             (put 'suspend-frame 'disabled t)
                             

                             ;;use UTF-8.
                             (prefer-coding-system 'utf-8)

                             ;; Nicer handling of regions.
                             (transient-mark-mode 1)


                             ;; Enable highlighting of current line.
                             ;;(global-hl-line-mode 1)

                             ;; Avoid noisy bell.
                             (setq visible-bell t)

                             ;; Enable indentation+completion using the TAB key.
                             ;; `completion-at-point' is often bound to M-TAB.
                             (setq tab-always-indent 'complete)
                             (setq xref-search-program 'ripgrep)

                             (defun my/locate-hm-init ()
                             "Locate hm-init file"
                             (interactive)
                             (let ((first-path (seq-find (lambda (s) (string-prefix-p "/nix/store/" s)) load-path)))
                             (if first-path (find-alternate-file (concat (car (split-string first-path "/share/emacs/site-lisp")) "/share/emacs/site-lisp/hm-init.el"))
                             (error "cannot find /nix/store in load-path"))))

                             (defun prot/keyboard-quit-dwim ()
                                  "Do-What-I-Mean behaviour for a general `keyboard-quit'.
                                  The generic `keyboard-quit' does not do the expected thing when
                                  the minibuffer is open.  Whereas we want it to close the
                                  minibuffer, even without explicitly focusing it.
                                  The DWIM behaviour of this command is as follows:

                                  - When the region is active, disable it.
                                  - When a minibuffer is open, but not focused, close the minibuffer.
                                  - When theCompletions buffer is selected, close it.
                                  - In every other case use the regular `keyboard-quit'."
                              (interactive)
                              (cond
                               ((region-active-p)
                                (keyboard-quit))
                                ((derived-mode-p 'completion-list-mode)
                                (delete-completion-window))
                               ((> (minibuffer-depth) 0)
                               (abort-recursive-edit))
                               (t
                               (keyboard-quit))))

                               (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

                               (defun add-all-to-list (var &rest elems)
              (dolist (elem (reverse elems))
              (add-to-list var elem)))
            '';

            postlude = ''
                            ;; Minimising & quitting Emacs way too many times without wanting to.
                            (keycast-header-line-mode)

                                          (repeat-mode 1)

              	                          ;; add smerge-basic-map to repeat-map

                           (with-eval-after-load 'smerge-mode               
                                        (map-keymap
                            (lambda (_key cmd)
                              (when (symbolp cmd)
                                (put cmd 'repeat-map 'smerge-basic-map)))
                                smerge-basic-map))  

            '';

            usePackage = {
              # pratice emacs-fu
              disable-mouse = {
                enable = true;
                config = ''
                  ;;(global-disable-mouse-mode)
                '';
              };
              gcmh = {
                enable = true;
                defer = 1;
                diminish = [ "gcmh-mode" ];
                command = [ "gcmh-mode" ];
                config = ''
                  (setq gcmh-idle-delay 'auto)
                  (gcmh-mode)
                '';
              };
              browse-kill-ring = {
                enable = true;
                command = [ "browse-kill-ring" ];
              };

              emacs-everywhere = {
                enable = true;
                config = ''
                  (setq emacs-everywhere--dir
                           (locate-user-emacs-file "everywhere")
                  )
                '';
              };

              god-mode = {
                enable = false;
                config = ''
                     (require 'god-mode-isearch)
                     (setq-default cursor-type 'bar)

                     (blink-cursor-mode -1)

                     ;; Functions
                     (defun god-update-cursor ()
                         "Update my cursor."
                         (setq cursor-type
                               (if god-local-mode
                                   'box
                                   'bar)))

                    ;;(global-set-key (kbd "z") #'god-mode-all)
                    (define-key god-local-mode-map (kbd "i") 'god-mode-all)
                    (define-key god-local-mode-map (kbd ".") 'repeat)
                   ;; (god-mode-all)
                    (add-hook 'god-mode-enabled-hook 'god-update-cursor)
                    (add-hook 'god-mode-disabled-hook 'god-update-cursor)
                    ;;(global-set-key "\C-x\ b" #'list-buffer)
                  ;;  (global-set-key "\C-x\ d" #'list-directory)
                  ;;  (global-set-key "\C-x\C-u" #'undo)
                  ;;  (global-set-key "\C-x\ u" #'upcase-region)
                  ;;  (global-set-key "\C-x\C-b" #'switch-to-buffer) ;; list-buffer with meow is little odd
                  ;;  (global-set-key "\C-x\C-d" #'dired) ;; list-directory with meow is little odd
                '';
              };
              # steal modeline from prot
              prot-modeline = {
                enable = true;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "prot-modeline";
                    version = "0.0.1";
                    src = ./packages/prot-modeline.el;
                    packageRequires = [
                      (epkgs.trivialBuild {
                        pname = "prot-common";
                        version = "0.0.1";
                        src = ./packages/prot-common.el;
                      })
                    ];
                  };
                config = ''
                  (setq mode-line-compact nil) ; Emacs 28
                  (setq mode-line-right-align-edge 'right-margin) ; Emacs 30
                  (setq-default mode-line-format
                       '("%e"
                         prot-modeline-kbd-macro
                         prot-modeline-narrow
                         prot-modeline-buffer-status
                         prot-modeline-window-dedicated-status
                         prot-modeline-input-method
                         "  "
                         prot-modeline-buffer-identification
                         "  "
                         prot-modeline-major-mode
                         "  "
                         mode-line-position
                         "  "
                         prot-modeline-process
                         "  "
                         prot-modeline-vc-branch
                         "  "
                         prot-modeline-eglot
                         "  "
                         prot-modeline-flymake
                         "  "
                         mode-line-format-right-align ; Emacs 30
                         prot-modeline-notmuch-indicator
                         "  "
                         prot-modeline-misc-info))
                                  
                  (with-eval-after-load 'spacious-padding
                    (defun prot/modeline-spacious-indicators ()
                      "Set box attribute to `'prot-modeline-indicator-button' if spacious-padding is enabled."
                      (if (bound-and-true-p spacious-padding-mode)
                          (set-face-attribute 'prot-modeline-indicator-button nil :box t)))

                    ;; Run it at startup and then afterwards whenever
                    ;; `spacious-padding-mode' is toggled on/off.
                    (prot/modeline-spacious-indicators)
                    (add-hook 'spacious-padding-mode-hook #'prot/modeline-spacious-indicators))
                '';
              };
              autorevert = {
                enable = true;
                hook = [
                  "(after-init . global-auto-revert-mode)"
                  "(dire-mode . auto-revert-mode)"
                ];
                custom = ''
                  (auto-revert-use-notify nil)
                  (auto-revert-verbose t)
                '';
              };

              ultra-scroll = {
                enable = true;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "ultra-scroll";
                    version = "0-unstable-2025-04-17";
                    src = pkgs.fetchFromGitHub {
                      owner = "jdtsmith";
                      repo = "ultra-scroll";
                      rev = "f2e4fba601b6116f6f0bcb73cadf897bd8f7b764";
                      #sha256 = lib.fakeSha256;
                      sha256 = "sha256-Dgt7eE4a1gi7iYAxLhfPbmo3Jglq97DJopf2i+Ft7vI=";
                    };
                  };
                hook = [
                  "(after-init . ultra-scroll-mode)"
                ];
                custom = ''
                  (scroll-conservatively 101)
                  (scroll-margin 0)
                '';
              };
              ask-mode = {
                enable = config.modules.dev.ask.enable;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/ask-mode {
                    inherit (pkgs) haskellPackages;
                    inherit (epkgs)
                      melpaBuild
                      ;
                  });
                mode = [
                  ''"\\.ask\\'"'' # \
                ];
                config = "(require 'ask-mode)";
              };
              hurl-mode = {
                enable = true;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/hurl-mode.nix {
                    inherit (pkgs)
                      fetchFromGitHub
                      replaceVars
                      writeText
                      unstableGitUpdater
                      ;
                    inherit lib;
                    inherit (epkgs) melpaBuild;
                  });
                extraPackages = [
                  pkgs.hurl
                ];

              };
              auto-save = {
                enable = true;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "auto-save";
                    version = "0.0.1";
                    src = pkgs.fetchFromGitHub {
                      owner = "manateelazycat";
                      repo = "auto-save";
                      rev = "0fb3c0f38191c0e74f00bae6adaa342de3750e83";
                      sha256 = "sha256-MCa28kGMBKLA/WqcDgJVtbul//R80nwWuI757wc12KI=";
                    };
                    preferLocalBuild = true;
                    allowSubstitutes = false;
                  };
                config = ''
                  (auto-save-enable)
                  (setq auto-save-silent t)   ; quietly save
                  ;;; custom predicates if you don't want auto save.
                  ;;; disable auto save mode when current filetype is an gpg file.
                  (setq auto-save-disable-predicates
                        '((lambda ()
                        (string-suffix-p
                        "gpg"
                        (file-name-extension (buffer-name)) t))))
                '';
              };

              expand-region = {
                enable = true;
                bind = {
                  "C-=" = "er/expand-region";
                };
              };
              # vim like
              change-inner = {
                enable = true;
                bind = {
                  "M-i" = "change-inner";
                  "M-o" = "change-outer";
                };
              };
              ## Remember where we where in a previously visited file. Built-in package.
              saveplace = {
                enable = true;
                defer = 1;
                config = ''
                  (setq-default save-place t)
                  (setq save-place-file (locate-user-emacs-file "places"))
                  (save-place-mode 1)
                '';
              };

              recentf = {
                enable = true;
                hook = [ "(after-init . recentf-mode)" ];
                custom = ''
                  (recentf-auto-cleanup nil)
                  (recentf-max-saved-items 100)
                  (recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))

                '';
                command = [
                  "recentf-mode"
                  "recentf-add-file"
                  "recentf-apply-filename-handlers"
                ];
              };

              savehist = {
                enable = true;
                custom = ''
                  (history-delete-duplicates t)
                  (savehist-additional-variables
                   '(file-name-history
                     kmacro-ring
                     compile-history
                     compile-command))
                  (savehist-autosave-interval 60)
                  ;(savehist-file (user-data "history"))
                  (savehist-ignored-variables
                   '(load-history
                     flyspell-auto-correct-ring
                     org-roam-node-history
                     magit-revision-history
                     org-read-date-history
                     query-replace-history
                     yes-or-no-p-history
                     kill-ring))
                '';
                config = ''
                  (savehist-mode 1)
                '';
              };

              compile = {
                enable = true;
                bind = {
                  "C-c c" = "compile";
                  "M-O" = "show-compilation";
                };
                bindLocal.compilation-mode-map = {
                  "z" = "delete-window";
                };
                hook = [
                  "(compilation-filter . compilation-ansi-color-process-output)"
                ];
                custom = ''
                  (compilation-always-kill t)
                  (compilation-ask-about-save nil)
                  (compilation-context-lines 10)
                  (compilation-scroll-output 'first-error)
                  (compilation-skip-threshold 2)
                  (compilation-window-height 100)
                '';
                preface = ''
                                  (defun show-compilation ()
                    (interactive)
                    (let ((it
                           (catch 'found
                             (dolist (buf (buffer-list))
                               (when (string-match "\\*compilation\\*" (buffer-name buf))
                                 (throw 'found buf))))))
                      (if it
                          (display-buffer it)
                        (call-interactively 'compile))))

                  (defun compilation-ansi-color-process-output ()
                    (ansi-color-process-output nil)
                    (set (make-local-variable 'comint-last-output-start)
                         (point-marker)))
                '';
              };

              # stealed from https://www2.lib.uchicago.edu/keith/emacs/init.el
              ediff = {
                enable = true;
                config = ''
                  ;; don't use a separate Frame for the control panel
                  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
                  ;; horizontal split is more readable
                  (setq ediff-split-window-function 'split-window-horizontally)

                  ;; restore window config upon quitting ediff
                  (defvar ue-ediff-window-config nil "Window config before ediffing.")
                  (add-hook 'ediff-before-setup-hook
                  (lambda ()
                  (setq ue-ediff-window-config (current-window-configuration))))
                  (dolist (hook '(ediff-suspend-hook ediff-quit-hook))
                  (add-hook hook
                  (lambda ()
                  (set-window-configuration ue-ediff-window-config))))
                '';
              };

              dired = {
                enable = true;
                command = [
                  "dired"
                  "dired-jump"
                ];
                config = ''
                  (put 'dired-find-alternate-file 'disabled nil)
                  (setq dired-use-ls-dired nil)
                  ;; Be smart about choosing file targets.
                  (setq dired-dwim-target t)
                  (setq dired-auto-revert-buffer t)
                  ;; Use the system trash can.
                  (setq delete-by-moving-to-trash t)
                  (setq dired-listing-switches "-alvh --group-directories-first")
                '';
              };

              reveal-in-osx-finder = {
                enable = isDarwin;
                noRequire = true;
                preface = ''
                  (defun my/darwin-open ()
                      (interactive)
                      (call-process "/usr/bin/open" nil nil nil
                                    "-R" (expand-file-name
                                          (or (buffer-file-name)
                                          default-directory))))
                '';
                bind = {
                  "C-c M-v" = "my/darwin-open";
                };
              };
              auth-source-pass = {
                enable = true;
                extraConfig = ''
                  :preface
                  (defvar auth-source-pass--cache (make-hash-table :test #'equal))

                  (defun auth-source-pass--reset-cache ()
                    (setq auth-source-pass--cache (make-hash-table :test #'equal)))

                  (defun auth-source-pass--read-entry (entry)
                    "Return a string with the file content of ENTRY."
                    (run-at-time 45 nil #'auth-source-pass--reset-cache)
                    (let ((cached (gethash entry auth-source-pass--cache)))
                      (or cached
                          (puthash
                           entry
                           (with-temp-buffer
                             (insert-file-contents (expand-file-name
                                                    (format "%s.gpg" entry)
                                                    (getenv "PASSWORD_STORE_DIR")))
                             (buffer-substring-no-properties (point-min) (point-max)))
                           auth-source-pass--cache))))

                  (defun auth-source-pass-entries ()
                    "Return a list of all password store entries."
                    (let ((store-dir (getenv "PASSWORD_STORE_DIR")))
                      (mapcar
                       (lambda (file) (file-name-sans-extension (file-relative-name file store-dir)))
                       (directory-files-recursively store-dir "\.gpg$"))))
                  :config
                  (auth-source-pass-enable)
                '';
              };

              wdired = {
                enable = true;
                bindLocal = {
                  dired-mode-map = {
                    "C-c C-w" = "wdired-change-to-wdired-mode";
                  };
                };
                config = ''
                  ;; I use wdired quite often and this setting allows editing file
                  ;; permissions as well.
                  (setq wdired-allow-to-change-permissions t)
                '';
              };

              # Hide hidden files when opening a dired buffer. But allow showing them by
              # pressing `.`.
              dired-x = {
                enable = true;
                hook = [ "(dired-mode . dired-omit-mode)" ];
                bindLocal.dired-mode-map = {
                  "." = "dired-omit-mode";
                };
                config = ''
                  (setq dired-omit-verbose nil
                        dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
                '';
              };

              dired-subtree = {
                enable = true;
                after = [ "dired" ];

              };

              trashed = {
                enable = true;
                command = [ "trashed" ];
                custom = ''
                  (trashed-action-confirmer 'y-or-n-p)
                  (trashed-use-header-line t)
                  (trashed-sort-key '("Date deleted" . t))
                  (trashed-date-format "%Y-%m-%d %H:%M:%S")
                '';
              };

              eyebrowse = {
                enable = true;
                custom = ''
                  (eyebrowse-keymap-prefix "�")
                  (eyebrowse-mode-line-separator " ")
                  (eyebrowse-new-workspace t)
                '';
                config = ''
                  (eyebrowse-mode t)
                '';
              };

              #             (use-package eyebrowse
              # :bind-keymap ("C-\\" . eyebrowse-mode-map)
              # :bind (:map eyebrowse-mode-map
              #             ("C-\\ C-\\" . eyebrowse-last-window-config)
              #             ("A-1" . eyebrowse-switch-to-window-config-1)
              #             ("A-2" . eyebrowse-switch-to-window-config-2)
              #             ("A-3" . eyebrowse-switch-to-window-config-3)
              #             ("A-4" . eyebrowse-switch-to-window-config-4))
              # :custom
              # (eyebrowse-keymap-prefix "�")
              # (eyebrowse-mode-line-separator " ")
              # (eyebrowse-new-workspace t)
              # :config
              # (eyebrowse-mode t))

              keycast = {
                enable = true;
                after = [ "prot-modeline" ];
                command = [
                  "keycast-mode-line-mode"
                  "keycast-header-line-mode"
                  "keycast-tab-bar-mode"
                  "keycast-log-mode"
                ];
                init = ''
                  (setq keycast-mode-line-format "%2s%k%c%R")
                  (setq keycast-mode-line-insert-after 'prot-modeline-vc-branch)
                  (setq keycast-mode-line-window-predicate 'mode-line-window-selected-p)
                  (setq keycast-mode-line-remove-tail-elements nil)
                '';
                config = ''
                  (dolist (input '(self-insert-command org-self-insert-command))
                  (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

                  (dolist (event '(mouse-event-p
                                   mouse-movement-p
                                   mwheel-scroll handle-select-window
                                   mouse-set-point mouse-drag-region))
                                   (add-to-list 'keycast-substitute-alist `(,event nil)))

                '';

              };
              # https://melpa.org/#/dot-mode
              #`C-.’ is bound to dot-mode-execute, which executes the buffer of stored commands as a keyboard macro.
              #`C-M-.’ is bound to dot-mode-override, which will cause dot-mode to remember the next keystroke regardless of whether it changes the buffer and regardless of the value of the dot-mode-ignore-undo variable.

              #`C-c-.’ is bound to dot-mode-copy-to-last-kbd-macro, which will copy the current dot mode keyboard macro to the last-kbd-macro variable. It can then be executed via call-last-kbd-macro (normally bound to `C-x-e’), named via name-last-kbd-macro, and then inserted into your .emacs via insert-kbd-macro.
              dot-mode = {
                enable = true;
                command = [
                  "dot-mode"
                  "dot-mode-execute"
                ];
                diminish = [ "dot-mode" ];
                bind = {
                  "C-." = "dot-mode-execute";
                  #"C-M-." = "dot-mode-override";
                  #"C-c-. ," = "dot-mode-copy-to-last-kbd-macro";
                };
                config = ''
                  (global-dot-mode t)
                '';
              };

              free-keys = {
                enable = false;
                defer = 1;
                command = [ "free-keys" ];
              };

              nerd-icons = {
                enable = true;
                custom = ''
                  (nerd-icons-font-family "PragmataPro Mono Liga")
                '';
              };

              nerd-icons-completion = {
                enable = true;
                after = [ "marginalia" ];
                config = ''
                  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
                '';
              };
              nerd-icons-corfu = {
                enable = true;
                after = [ "corfu" ];
                config = ''
                  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
                '';
              };
              nerd-icons-dired = {
                enable = true;
                hook = [
                  "(dired-mode . nerd-icons-dired-mode)"
                ];
              };

              evil = {
                enable = true;
                command = [ "evil-mode" ];
              };

              marginalia = {
                enable = true;
                config = "(marginalia-mode)";
              };

              orderless = {
                enable = true;
                demand = true;
                config = ''
                  (setq completion-styles '( basic substring orderless))
                  (setq read-file-name-completion-ignore-case t)
                  (setq completion-category-overrides
                   '((file (styles basic partial-completion))))
                '';
              };

              vertico = {
                enable = true;
                after = [ "cape" ];
                demand = true;
                custom = ''
                  (vertico-count 10)
                  (vertico-resize nil)
                  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
                  (vertico-cycle t)
                '';
                bind = {
                  "C-c . ." = "vertico-repeat";
                };
                bindLocal.vertico-map = {
                  "C-j" = "vertico-exit-input";
                  "C-M-n" = "vertico-next-group";
                  "C-M-p" = "vertic-previous-group";

                };
                hook = [
                  "(minibuffer-setup . vertico-repeat-save)"
                  "(rfn-eshadow-update-overlay . vertico-directory-tidy)"
                ];
                preface = ''
                                  (defun crm-indicator (args)
                    (cons (format "[CRM%s] %s"
                                  (replace-regexp-in-string
                                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                                   crm-separator)
                                  (car args))
                          (cdr args)))

                  (defsubst time-greater-p (a b)
                    (time-less-p b a))

                  (defun my/sort-by-mtime (files)
                    "Sort FILES by modification time (newest first)."
                    (sort-on files
                             #'time-greater-p
                             #'(lambda (a) (file-attribute-modification-time (file-attributes a)))))
                '';
                config = ''
                                   (vertico-mode)
                                   ;; Different scroll margin

                                     ;; Add prompt indicator to `completing-read-multiple'.
                  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
                  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

                    ;; Do not allow the cursor in the minibuffer prompt
                  (setq minibuffer-prompt-properties
                        '(read-only t cursor-intangible t face minibuffer-prompt))

                  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

                  ;; Hide commands in M-x which do not work in the current mode. Vertico
                  ;; commands are hidden in normal buffers.
                  (setq read-extended-command-predicate
                        #'command-completion-default-include-p)

                          (use-package vertico-repeat
                    :demand t)

                  (use-package vertico-directory
                    :demand t
                    :bind (:map vertico-map
                                ("<backspace>"   . vertico-directory-delete-char)
                                ("C-w"           . vertico-directory-delete-word)
                                ("C-<backspace>" . vertico-directory-delete-word)
                                ;; ("RET" .           vertico-directory-enter)
                                ))

                  (use-package vertico-quick
                    :demand t
                    :bind (:map vertico-map
                                ("C-."   . vertico-quick-exit)
                                ("C-M-." . vertico-quick-embark))
                    :preface
                    (defun vertico-quick-embark (&optional arg)
                      "Embark on candidate using quick keys."
                      (interactive)
                      (when (vertico-quick-jump)
                        (embark-act arg))))

                  (use-package vertico-multiform
                    :demand t
                    :bind (:map vertico-map
                                ("C-i"   . vertico-multiform-vertical)
                                ("<tab>" . vertico-insert)))
                        

                '';
              };

              # try nested in vertico for use
              # vertico-repeat = {
              #   enable = true;
              #   demand = true;
              #   bind = {
              #     "C-c . ." = "vertico-repeat";
              #   };
              #   config = "(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)";
              # };

              # vertico-multiform = {
              #   enable = true;
              #   demand = true;
              #   config = ''
              #     (require 'vertico-grid)
              #     (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
              #     (vertico-multiform-mode)
              #   '';
              # };

              goggles = {
                enable = true;
                hook = [ "((prog-mode text-mode) . goggles-mode)" ];
                config = ''
                  (setq-default goggles-pulse t) ;; set to nil to disable pulsing
                '';
              };

              avy = {
                enable = true;
                bind = {
                  "C-j" = "avy-goto-char-timer";
                  "C-f" = "avy-goto-char-in-line";
                  "M-g y" = "avy-copy-line";
                  "M-g M-y" = "avy-copy-region";
                  "M-g M-p" = "avy-goto-line-above";
                  "M-g M-n" = "avy-goto-line-below";
                  "M-g C-w" = "avy-kill-region";
                  "M-g M-w" = "avy-kill-ring-save-region";
                };
                config = ''
                  (setq avy-keys '(?e ?h ?a ?o ?v ?r
                    ?s ?g ?w ?p
                    ?c ?f ?d))
                '';
              };
              embark = {
                enable = true;
                command = [ "embark-prefix-help-command" ];
                bind = {
                  "M-." = "embark-act";
                  #"M-." = "embark-dwim";
                  "C-h B" = "embark-bindings";
                };
                init = ''
                  (setq embark-help-key "?")
                  (setq prefix-help-command #'embark-prefix-help-command)
                '';
                config = ''
                  (defun embark-which-key-indicator ()
                    "An embark indicator that displays keymaps using which-key.
                  The which-key help message will show the type and value of the
                  current target followed by an ellipsis if there are further
                  targets."
                    (lambda (&optional keymap targets prefix)
                      (if (null keymap)
                          (which-key--hide-popup-ignore-command)
                        (which-key--show-keymap
                         (if (eq (plist-get (car targets) :type) 'embark-become)
                             "Become"
                           (format "Act on %s '%s'%s"
                                   (plist-get (car targets) :type)
                                   (embark--truncate-target (plist-get (car targets) :target))
                                   (if (cdr targets) "…" "")))
                         (if prefix
                             (pcase (lookup-key keymap prefix 'accept-default)
                               ((and (pred keymapp) km) km)
                               (_ (key-binding prefix 'accept-default)))
                           keymap)
                         nil nil t (lambda (binding)
                                     (not (string-suffix-p "-argument" (cdr binding))))))))

                  (setq embark-indicators
                    '(embark-which-key-indicator
                      embark-highlight-indicator
                      embark-isearch-highlight-indicator))

                  (defun embark-hide-which-key-indicator (fn &rest args)
                    "Hide the which-key indicator immediately when using the completing-read prompter."
                    (which-key--hide-popup-ignore-command)
                    (let ((embark-indicators
                           (remq #'embark-which-key-indicator embark-indicators)))
                        (apply fn args)))

                  (advice-add #'embark-completing-read-prompter
                  :around #'embark-hide-which-key-indicator)

                '';
              };

              corfu = {
                enable = cfg.lspStyle != "lsp-bridge";
                hook = [ "(after-init . global-corfu-mode)" ];
                extraConfig = ''
                    :custom
                  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
                  (corfu-auto t)                 ;; Enable auto completion
                  (corfu-separator ?\s)          ;; Orderless field separator
                  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
                  (corfu-quit-no-match t)       ;; Never quit, even if there is no match
                  (corfu-preview-current nil)    ;; Disable current candidate preview
                  (corfu-preselect 'prompt)      ;; Preselect the prompt
                  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
                  (corfu-scroll-margin 5)        ;; Use scroll margin
                '';
                config = ''
                  (setq tab-always-indent 'complete)
                  (setq corfu-preview-current nil)
                  (setq corfu-min-width 20)
                  (setq corfu-popupinfo-delay '(1.25 . 0.5))
                  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

                  ;; Sort by input history (no need to modify `corfu-sort-function').
                  (with-eval-after-load 'savehist
                    (corfu-history-mode 1)
                    (add-to-list 'savehist-additional-variables 'corfu-history))
                '';
              };

              consult = {
                enable = true;
                hook = [ "(completion-list-mode . consult-preview-at-point-mode)" ];
                extraConfig = ''
                  :bind (;; C-c bindings in `mode-specific-map'
                  ("C-c M-x" . consult-mode-command)
                  ("C-c h" . consult-history)
                  ("C-c k" . consult-kmacro)
                  ("C-c m" . consult-man)
                  ("C-c i" . consult-info)
                  ([remap Info-search] . consult-info)
                  ;; C-x bindings in `ctl-x-map'
                  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
                  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
                  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
                  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
                  ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
                  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
                  ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
                  ;; Custom M-# bindings for fast register access
                  ("M-#" . consult-register-load)
                  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
                  ("C-M-#" . consult-register)
                  ;; Other custom bindings
                  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
                  ;; M-g bindings in `goto-map'
                  ("M-g e" . consult-compile-error)
                  ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
                  ("M-g g" . consult-goto-line)             ;; orig. goto-line
                  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
                  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
                  ("M-g m" . consult-mark)
                  ("M-g k" . consult-global-mark)
                  ("M-g i" . consult-imenu)
                  ("M-g I" . consult-imenu-multi)
                  ;; M-s bindings in `search-map'
                  ("M-s f" . consult-fd)                  ;; Alternative: consult-find
                  ("M-s c" . consult-locate)
                  ("M-s g" . consult-grep)
                  ("M-s G" . consult-git-grep)
                  ("M-s r" . consult-ripgrep)
                  ("M-s l" . consult-line)
                  ("M-s L" . consult-line-multi)
                  ("M-s k" . consult-keep-lines)
                  ("M-s u" . consult-focus-lines)
                  ;; Isearch integration
                  ("M-s e" . consult-isearch-history)
                  :map isearch-mode-map
                  ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
                  ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
                  ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
                  ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
                  ;; Minibuffer history
                  :map minibuffer-local-map
                  ("M-s" . consult-history)                 ;; orig. next-matching-history-element
                  ("M-r" . consult-history))
                '';

                custom = ''
                  (consult-narrow-key "<")
                '';

                init = ''
                  ;; Tweak the register preview for `consult-register-load',
                  ;; `consult-register-store' and the built-in commands.  This improves the
                  ;; register formatting, adds thin separator lines, register sorting and hides
                  ;; the window mode line.
                  (advice-add #'register-preview :override #'consult-register-window)
                  (setq register-preview-delay 0.5)

                  ;; Use Consult to select xref locations with preview
                  (setq xref-show-xrefs-function #'consult-xref
                        xref-show-definitions-function #'consult-xref)
                '';

                config = ''
                  (use-package consult-xref)          
                  (consult-customize
                   consult-theme
                   :preview-key '(:debounce 0.2 any)
                   consult-ripgrep
                   consult-git-grep
                   consult-grep
                   consult-man
                   consult-bookmark
                   consult-recent-file
                   consult-xref
                   consult--source-bookmark
                   consult--source-file-register
                   consult--source-recent-file
                   consult--source-project-recent-file
                   :preview-key '(:debounce 0.4 any))

                '';
              };

              consult-xref = {
                enable = true;
              };
              # embark act then press C
              embark-consult = {
                enable = true;
                hook = [ "(embark-collect-mode . consult-preview-at-point-mode)" ];
              };

              which-key = {
                enable = true;
                config = ''
                    (setq which-key-side-window-location 'bottom
                  	  which-key-sort-order #'which-key-key-order-alpha
                  	  which-key-sort-uppercase-first nil
                  	  which-key-add-column-padding 1
                  	  which-key-max-display-columns nil
                  	  which-key-side-window-slot -10
                  	  which-key-side-window-max-height 0.25
                  	  which-key-idle-delay 0.8
                      which-key--god-mode-support-enabled t
                  	  which-key-max-description-length 25
                  	  which-key-allow-imprecise-window-fit t
                  	  which-key-separator " → " )
                      (which-key-mode)
                '';
              };

              project = {
                enable = true;
                config = ''
                  (defun project-magit-status ()
                    "Run magit-status in the current project's root."
                    (interactive)
                    (magit-status-setup-buffer (project-root (project-current t))))

                  (setq project-switch-commands
                   '((?f "Find file" project-find-file)
                          (?g "Find regexp" project-find-regexp)
                          (?d "Dired" project-dired)
                          (?b "Buffer" project-switch-to-buffer)
                          (?q "Query replace" project-query-replace-regexp)
                          (?v "magit" project-magit-status)
                          (?k "Kill buffers" project-kill-buffers)
                          (?! "Shell command" project-shell-command)
                          (?e "Eshell" project-eshell)))
                '';
                after = [ "magit" ];
              };

              projectile = {
                enable = true;
                bindKeyMap = {
                  "C-c p" = "projectile-command-map";
                };
                custom = ''
                  (projectile-cache-file  (locate-user-emacs-file "projectile.cache"))
                  (projectile-enable-caching t)
                  (projectile-create-missing-test-files t)
                  (projectile-file-exists-local-cache-expire 300)
                '';
              };

              consult-projectile = {
                enable = true;
              };

              ace-window = {
                enable = true;
                command = [ "ace-window" ];
                bind = {
                  "C-x o" = "ace-window";
                };

              };

              winner = {
                enable = true;
                config = ''
                  (winner-mode 1)
                '';
              };
              org = {
                enable = true;
                demand = true;
                custom = ''
                  (org-todo-keywords
                   '((sequence "TODO(t@/!)"
                               "DOING(s!/!)"
                               "WAIT(w@/!)"
                               "DEFER(r!/!)"
                               "TASK(g@/!)"
                               "HABIT(h@/!)"
                               "|" "DONE(d@/!)" "CANCELED(x@/!)")
                     (sequence "|" "NOTE(n)" "LINK(l)" "FEEDBACK(f)")))
                  (org-todo-keyword-faces
                   `(
                     ("TODO"     :foreground ,palette-slate-blue      :weight bold)
                     ("DOING"    :foreground ,palette-yellow          :weight bold)
                     ("WAIT"     :foreground ,palette-red             :weight bold)
                     ("TASK"     :foreground ,palette-blue            :weight bold)
                     ("DEFER"    :foreground ,palette-slate-blue-dark :weight bold)
                     ("DONE"     :foreground ,palette-green-dark      :weight bold)
                     ("CANCELED" :foreground "grey50"                 :weight bold :strike-through t)
                     ("HABIT"    :foreground ,palette-orange          :weight bold)
                     ("LINK"     :foreground ,palette-orange-dark     :weight bold)
                     ("NOTE"     :foreground ,palette-red-dark        :weight bold)
                     ("FEEDBACK" :foreground ,palette-purple-dark     :weight bold)
                     ))
                  (org-todo-repeat-to-state "TODO")
                '';
                config = ''
                  (setq org-directory  "~/org/")
                  (setq org-agenda-files (append    
                                   (file-expand-wildcards (concat org-directory "agenda/*.org"))))
                  (setq  org-default-notes-file (concat org-directory "agenda/inbox.org"))
                  (defun my/org-entry-get-immediate (property)
                     (save-excursion
                     (let ((local (org--property-local-values property nil)))
                     (and local (mapconcat #'identity
                              (delq nil local)
                              (org--property-get-separator property))))))
                              
                  (org-babel-do-load-languages 'org-babel-load-languages
                  '(
                    (emacs-lisp . t)
                    (python . t)
                    (dot . t)
                    (scheme . t)
                    (racket . t)
                    (hurl .)
                    ))
                '';
              };
              geiser = {
                enable = true;
              };
              org-agenda = {
                enable = true;
                after = [ "org" ];
                defer = true;
                config = ''
                  ;; Set up agenda view.
                  (setq org-agenda-span 5
                        org-deadline-warning-days 14
                        org-agenda-show-all-dates t
                        org-agenda-skip-deadline-if-done t
                        org-agenda-skip-scheduled-if-done t
                        org-agenda-start-on-weekday nil)
                '';
              };
              org-habit = {
                enable = true;
                after = [ "org-agenda" ];
                custom = ''
                  (org-habit-preceding-days 42)
                  (org-habit-today-glyph 45)
                '';
                customFace = {
                  "org-habit-alert-face" = "((((background light)) (:background \"#f5f946\")))";
                  "org-habit-alert-future-face" = "((((background light)) (:background \"#fafca9\")))";
                  "org-habit-clear-face" = "((((background light)) (:background \"#8270f9\")))";
                  "org-habit-clear-future-face" = "((((background light)) (:background \"#d6e4fc\")))";
                  "org-habit-overdue-face" = "((((background light)) (:background \"#f9372d\")))";
                  "org-habit-overdue-future-face" = "((((background light)) (:background \"#fc9590\")))";
                  "org-habit-ready-face" = "((((background light)) (:background \"#4df946\")))";
                  "org-habit-ready-future-face" = "((((background light)) (:background \"#acfca9\")))";
                };
              };
              org-extra = {
                enable = true;
                after = [ "org" ];
                demand = true;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "org-extra";
                    version = "0.0.1";
                    src = ./packages/org-extra.el;
                    # elisp dependencies
                    packageRequires = [
                      epkgs.org
                      epkgs.org-ql
                      epkgs.dash
                    ];
                    preferLocalBuild = true;
                    allowSubstitutes = false;
                  };
              };
              org-refile = {
                enable = true;
                after = [ "org" ];
                custom = ''
                  (use-package org-extra
                      :functions (org-extra-refile-heading-p))
                  ;;(org-refile-target-verify-function #'org-extra-refile-heading-p)
                  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
                  (org-refile-use-cache nil)                  
                  (org-refile-use-outline-path 'file)
                  (org-outline-path-complete-in-steps nil)
                '';
              };
              ob-ipython = {
                enable = false;
              };
              ob-racket = {
                enable = true;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/ob-racket.nix {
                    inherit (pkgs)
                      fetchFromGitHub
                      replaceVars
                      writeText
                      unstableGitUpdater
                      ;
                    inherit lib;
                    inherit (epkgs) melpaBuild;
                  });
                after = [ "org" ];
              };
              aider = {
                enable = cfg.enableAider;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/aider.nix {
                    inherit (pkgs)
                      fetchFromGitHub
                      replaceVars
                      writeText
                      unstableGitUpdater
                      ;
                    inherit lib;
                    inherit (epkgs) melpaBuild;
                  });
                extraPackages = [
                  pkgs.aider-chat
                ];
                config = ''
                  (setq aider-args '("--no-auto-commits" "--model" "openrouter/deepseek/deepseek-coder"))
                  (setenv "OPENROUTER_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/openrouter/key.txt")
                               (string-trim (buffer-string))))
                '';
              };
              org-modern = {
                enable = true;
                after = [ "org" ];
                hook = [
                  "(org-mode . org-modern-mode)"
                  "(org-agenda-finalize . org-modern-agenda)"
                ];
              };
              org-appear = {
                enable = true;
                after = [ "org" ];
                hook = [
                  "(org-mode . org-appear-mode)"
                ];
              };
              org-download = {
                enable = true;
                command = [
                  "org-download-yank"
                  "org-download-clipboard"
                ];
                after = [ "org" ];
                custom = ''
                  (org-download-method 'directory)
                '';
                config = "(add-hook 'dired-mode-hook  'org-download-enable)";

                extraPackages =
                  if isDarwin then
                    [
                      pkgs.pngpaste
                    ]
                  else
                    [ ];

              };
              org-re-reveal = {
                enable = false;
                after = [ "org" ];
                config = ''
                  (setq org-re-reveal-root "${pkgs.reveal-js}/share")
                '';
              };
              org-noter = {
                enable = !(isDarwin);
                config = ''
                  (require 'org-noter-pdftools)
                '';
              };
              org-noter-pdftools = {
                after = [ "org-noter" ];
                config = ''
                    ;; Add a function to ensure precise note is inserted
                    (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
                      (interactive "P")
                      (org-noter--with-valid-session
                       (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                                     (not org-noter-insert-note-no-questions)
                                                                   org-noter-insert-note-no-questions))
                             (org-pdftools-use-isearch-link t)
                             (org-pdftools-use-freepointer-annot t))
                         (org-noter-insert-note (org-noter--get-precise-info)))))

                    ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
                    (defun org-noter-set-start-location (&optional arg)
                      "When opening a session with this document, go to the current location.
                  With a prefix ARG, remove start location."
                      (interactive "P")
                      (org-noter--with-valid-session
                       (let ((inhibit-read-only t)
                             (ast (org-noter--parse-root))
                             (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
                         (with-current-buffer (org-noter--session-notes-buffer session)
                           (org-with-wide-buffer
                            (goto-char (org-element-property :begin ast))
                            (if arg
                                (org-entry-delete nil org-noter-property-note-location)
                              (org-entry-put nil org-noter-property-note-location
                                             (org-noter--pretty-print-location location))))))))
                    (with-eval-after-load 'pdf-annot
                    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
                '';
              };

              org-pdftools = {
                enable = (!isDarwin);
                hook = [
                  "(org-mode . org-pdftools-setup-link)"
                ];
              };
              org-review = {
                enable = true;
              };
              dslide = {
                enable = false;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "dslide";
                    version = "0.5.1";
                    src = pkgs.fetchFromGitHub {
                      owner = "positron-solutions";
                      repo = "dslide";
                      rev = "145b06df68b3d584c491e76a300aead662c8271e";
                      sha256 = "sha256-NUxIaWcwmASxpHh7XlAahQLpwtYBj67jzeaeT5gkOLs=";
                    };
                    preferLocalBuild = true;
                    allowSubstitutes = false;
                  };

              };
              casual-suite = {
                enable = true;
                bind = {
                  "C-o" = "casual-editkit-main-tmenu";
                };
              };
              easy-kill = {
                enable = true;
                extraConfig = ''
                  :bind ([remap kill-ring-save] . easy-kill)
                  :bind ([remap mark-sexp] . easy-mark)
                '';
              };

              denote = {
                enable = true;
                after = [ "org" ];

                extraConfig = ''
                  :bind (("C-c n o" . denote-open-or-create))
                '';
                config = ''
                            (setq denote-directory "~/org/denote/")
                            (setq denote-templates
                                  `((report . "* Some heading\n\n* Another heading")
                                    (journal . ,(concat "* Tasks todo"
                                                     "\n\n"
                                                     "* TIL"
                                                     "\n\n"))))
                          
                            (denote-rename-buffer-mode)

                            (with-eval-after-load 'org-capture
                  (push '("n" "New note (With Denote)" plain (file denote-last-path) #'denote-org-capture :no-save t
                          :immediate-finish nil :kill-buffer t :jump-to-captured t ) org-capture-templates)
                  (
                   push '("d" "daily note" item (function denote-journal-new-or-existing-entry) "- %U %?" :prepend t) org-capture-templates)
                  )
                '';
              };

              denote-org = {
                enable = true;
                extraConfig = ''

                  :commands
                  ;; I list the commands here so that you can discover them more
                  ;; easily.  You might want to bind the most frequently used ones to
                  ;; the `org-mode-map'.
                  ( denote-org-link-to-heading
                    denote-org-backlinks-for-heading

                    denote-org-extract-org-subtree

                    denote-org-convert-links-to-file-type
                    denote-org-convert-links-to-denote-type

                    denote-org-dblock-insert-files
                    denote-org-dblock-insert-links
                    denote-org-dblock-insert-backlinks
                    denote-org-dblock-insert-missing-links
                    denote-org-dblock-insert-files-as-headings)
                '';
              };

              denote-journal = {
                enable = true;
                extraConfig = ''
                  ;; Bind those to some key for your convenience.
                  :commands ( denote-journal-new-entry
                              denote-journal-new-or-existing-entry
                              denote-journal-link-or-create-entry )
                  :hook (calendar-mode . denote-journal-calendar-mode)
                  :bind (("C-c n t" . denote-journal-new-or-existing-entry))
                  :config
                  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
                  ;; to nil to use the `denote-directory' instead.
                  (setq denote-journal-directory
                        (expand-file-name "journal" denote-directory))
                  ;; Default keyword for new journal entries. It can also be a list of
                  ;; strings.
                  (setq denote-journal-keyword "journal")
                  ;; Read the doc string of `denote-journal-title-format'.
                  ;;(setq denote-journal-title-format 'day-date-month-year)
                '';
              };
              consult-denote = {
                enable = true;
                after = [ "consult" ];
                custom = ''
                  (consult-denote-find-command 'consult-fd)
                '';
                extraConfig = ''
                                    
                  :bind
                  (("C-c n f" . consult-denote-find)
                   ("C-c n r" . consult-denote-grep))
                  :config
                  (consult-denote-mode 1)
                '';
              };

              smartparens = {
                enable = true;
                defer = 3;
                command = [
                  "smartparens-global-mode"
                  "show-smartparens-global-mode"
                ];
                bindLocal = {
                  smartparens-mode-map = {
                    "M-<right>" = "sp-forward-sexp";
                    "M-<left>" = "sp-backward-sexp";
                  };
                };
                config = ''
                  (require 'smartparens-config)
                  (smartparens-global-mode t)
                  (show-smartparens-global-mode t)
                '';
              };

              # https://takeonrules.com/2024/03/01/quality-of-life-improvement-for-entering-and-exiting-magit/
              magit = {
                enable = true;
                bind = {
                  "C-x g" = "magit-status";
                };
                bindLocal = {
                  magit-mode-map = {
                    "U" = "magit-unstage-all";
                    "k" = "magit-discard";
                  };
                };
                command = [ "magit-project-status" ];
                config = ''
                  (setq magit-list-refs-sortby "-committerdate")
                  (setq forge-add-pullreq-refspec 'ask)
                  (add-to-list 'git-commit-style-convention-checks
                  'overlong-summary-line)
                  (setq magit-display-buffer-function
                   #'magit-display-buffer-fullframe-status-v1)
                  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
                '';
              };
              flycheck = {
                enable = true;
                config = ''
                  (global-flycheck-mode)
                  (flycheck-define-checker vale
                  "A checker for prose"
                  :command ("vale" "--output" "line" source)
                  :standard-input nil
                  :error-patterns
                  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
                  :modes (markdown-mode gfm-mode org-mode text-mode))
                  (add-to-list 'flycheck-checkers 'vale 'append)
                '';
              };

              jinx = {
                enable = true;
                diminish = [ "jinx-mode" ];
                command = [ "jinx-mode" ];
                hook = [ "(org-mode . jinx-mode)" ];
                #bind = {
                #  "M-$" = "jinx-correct";
                #  "C-M-$" = "jinx-languages ";
                #};
                config = ''
                  (setq jinx-languages "en_CA")
                '';
              };
              pdf-tools = {
                enable = true;
                custom = ''
                  (pdf-tools-handle-upgrades nil)
                '';
                config = ''
                  (dolist
                  (pkg
                  '(pdf-annot pdf-cache pdf-dev pdf-history pdf-info pdf-isearch
                   pdf-links pdf-misc pdf-occur pdf-outline pdf-sync
                   pdf-util pdf-view pdf-virtual))
                  (require pkg))
                  (pdf-tools-install)
                '';
              };

              yaml-ts-mode = {
                enable = true;
              };
              nix-mode = {
                enable = true;
                hook = [
                  "(nix-mode . subword-mode) "
                ];
                config = ''(setq nix-indent-function 'nix-indent-line) '';
              };

              # https://github.com/jwiegley/dot-emacs/blob/master/init.org#haskell-mode
              haskell-mode = {
                enable = true;
                hook = [ "(haskell-mode . subword-mode)" ];
                mode = [
                  ''("\\.hs\\'" . haskell-mode)''
                  ''("\\.hsc\\'" . haskell-mode)''
                  ''("\\.c2hs\\'" . haskell-mode)''
                  ''("\\.cpphs\\'" . haskell-mode)''
                  ''("\\.lhs\\'" . haskell-literate-mode)''
                ];
              };
              agda2-mode = {
                enable = config.modules.dev.agda.enable;
              };
              dart-mode = {
                enable = config.modules.dev.dart.enable;
                mode = [
                  ''"\\.dart\\'"''
                ];
              };
              java-mode = {
                enable = true;
                preface = ''
                  (defun my/format-java ()
                      (interactive)
                      (call-process "${pkgs.google-java-format}/bin/google-java-format" nil nil nil
                                    "-r" (expand-file-name
                                          buffer-file-name
                                          ))
                       (revert-buffer-quick)
                                          )
                '';

                bindLocal = {
                  java-mode-map = {
                    "C-c C-f" = "my/format-java";
                  };
                };
                mode = [
                  ''"\\.java\\'"'' # \
                ];
              };
              kotlin-mode = {
                enable = true;
                config = "(require 'kotlin-mode)";
              };
              groovy-mode = {
                enable = true;
                mode = [
                  ''"\\.gradle\\'"'' # \
                  ''"\\.groovy\\'"'' # \
                  ''"Jenkinsfile\\'"'' # \
                ];
              };
              plantuml-mode = {
                enable = true;
              };
              protobuf-mode = {
                enable = true;
              };
              racket-mode = {
                enable = true;
              };
              terraform-mode = {
                enable = true;
                custom = "(terraform-indent-level 4)";
                config = ''
                  (defun my-terraform-mode-init ()
                    ;; if you want to use outline-minor-mode
                       (outline-minor-mode 1)
                    )

                  (add-hook 'terraform-mode-hook 'my-terraform-mode-init)
                '';
              };
              idris2-mode = {
                enable = config.modules.dev.idris2.enable;
              };

              popper = {
                enable = true;
                bind = {
                  "C-`" = "popper-toggle";
                  "M-`" = "popper-cycle";
                  "C-M-`" = "popper-toggle-type";
                };
                command = [
                  "popper-mode"
                  "popper-group-by-project"
                ];
                config = ''
                  (setq popper-reference-buffers
                          '("
                    Output\\ * $"
                            "\\ * Async Shell Command\\ * "
                            "\\ * Buffer List\\ * "
                            "\\ * Flycheck errors\\ * "
                            "\\ * Messages\\ * "
                            compilation-mode
                            help-mode
                            vterm-mode)
                        popper-group-function #'popper-group-by-project)
                  (popper-mode)
                '';
              };
              direnv = {
                enable = true;
                functions = [ "direnv--maybe-update-environment" ];
                preface = ''
                   (defconst emacs-binary-path (directory-file-name
                                               (file-name-directory
                                                (executable-find "emacsclient"))))

                  (defun patch-direnv-environment (&rest _args)
                    (let ((dir (file-name-as-directory emacs-binary-path)))
                      (unless (member dir exec-path)
                        (setenv "PATH" (concat emacs-binary-path ":" (getenv "PATH")))
                        (setq exec-path (cons dir exec-path)))))

                  (defvar my-direnv-last-buffer nil)

                  (defun my-direnv-maybe-update (&rest _ignore)
                    (unless (eq (current-buffer) my-direnv-last-buffer)
                      (setq my-direnv-last-buffer (current-buffer))
                      (direnv--maybe-update-environment)))
                '';
                init = ''
                  (advice-add 'direnv-update-directory-environment
                              :after #'patch-direnv-environment)
                  (add-hook 'change-major-mode-hook #'my-direnv-maybe-update)
                  ;; (add-hook 'buffer-list-update-hook #'my-direnv-maybe-update)
                  (add-hook 'window-selection-change-functions #'my-direnv-maybe-update)
                '';
              };
              editorconfig = {
                enable = true;
                config = ''
                  (editorconfig-mode 1)
                '';
              };
              envrc = {
                enable = false;

                hook = [ "(after-init . envrc-global-mode)" ];
              };

              just-mode.enable = true;
              justl = {
                enable = true;
                command = [ "justl-exec-recipe" ];
              };

              markdown-mode = {
                enable = true;
                mode = [
                  ''("\\`README\\.md\\'" . gfm-mode)''
                  ''("\\.md\\'"          . markdown-mode)''
                  ''("\\.markdown\\'"    . markdown-mode)''
                ];
                config = ''
                  (setq markdown-command "pandoc -f markdown_github+smart")
                  (setq markdown-command-needs-filename t)
                  (setq markdown-enable-math t)
                  (setq markdown-open-command "marked")
                  ;;:custom-face
                  ;;(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
                  ;;(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
                  ;;(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
                  ;;(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
                '';
              };

              eglot = {
                enable = cfg.lspStyle == "eglot";
                # https://emacs-china.org/t/eglot-lsp-server-command/29566
                preface = ''
                                    (defun my/eglot-enable-command-provider (orig-fn server)
                      "Unconditionally add :executeCommandProvider to Eglot client capabilities."
                      (let ((original-capabilities (funcall orig-fn server)))
                        ;; Add or update :executeCommandProvider at the top level
                        (plist-put original-capabilities
                                   :executeCommandProvider '(:commands (:dynamicRegistration :json-false)))))
                    (advice-add 'eglot-client-capabilities :around #'my/eglot-enable-command-provider)

                    (defun my/eglot-execute-command (command)
                      "Interactively execute a COMMAND supported by the current Eglot LSP server.
                  COMMAND is a string as advertised by the server. No arguments are passed."
                      (interactive
                       (let* ((server (eglot-current-server))
                              (caps (eglot--capabilities server))
                              (provider (plist-get caps :executeCommandProvider))
                              (commands (and provider (plist-get provider :commands))))
                         (list (completing-read "LSP Command: "
                                                (or (cl-coerce commands 'list) '())
                                                nil nil))))
                      (eglot-execute (eglot-current-server) (list :command command)))
                '';
                config = ''
                  (setq eglot-autoshutdown t)
                  (add-to-list 'eglot-server-programs
                            '((java-mode java-ts-mode) . ("jdtls"
                                 "--jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar"))
                                    )
                  ;;(add-hook 'kotlin-mode-hook #'eglot-ensure)
                  ;;(add-hook 'java-mode-hook #'eglot-ensure)
                  ;;(add-hook 'java-ts-mode-hook #'eglot-ensure)
                '';
              };

              eglot-booster = {
                enable = cfg.lspStyle == "eglot";
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "eglot-booster";
                    version = "0-unstable-2025-04-28";

                    src = pkgs.fetchFromGitHub {
                      owner = "jdtsmith";
                      repo = "eglot-booster";
                      rev = "1260d2f7dd18619b42359aa3e1ba6871aa52fd26";
                      #sha256 = lib.fakeSha256;
                      sha256 = "sha256-teAKWDDL7IrCBiZUVIVlB3W22G9H6IrWiRV/P62dFy0=";
                    };
                    preferLocalBuild = true;
                    allowSubstitutes = false;
                  };
                after = [ "eglot" ];
                extraPackages = [
                  pkgs.emacs-lsp-booster
                ];
                config = ''
                  	     (eglot-booster-mode)
                '';
              };

              lsp-ui = {
                enable = cfg.lspStyle == "lsp-mode";

                command = [ "lsp-ui-mode" ];
                bindLocal = {
                  lsp-mode-map = {
                    "C-c r d" = "lsp-ui-doc-toggle";
                    "C-c r i" = "lsp-ui-doc-focus-frame";
                    "C-c f s" = "lsp-ui-find-workspace-symbol";
                  };
                };
                config = ''
                  (setq lsp-ui-sideline-enable t
                        lsp-ui-sideline-show-symbol nil
                        lsp-ui-sideline-show-hover nil
                        lsp-ui-sideline-show-code-actions nil
                        lsp-ui-sideline-update-mode 'point)
                  (setq lsp-ui-doc-enable nil
                        lsp-ui-doc-position 'at-point
                        lsp-ui-doc-max-width 125
                        lsp-ui-doc-max-height 18)
                '';
              };

              lsp-ui-flycheck = {
                enable = cfg.lspStyle == "lsp-mode";

                after = [
                  "flycheck"
                  "lsp-ui"
                ];
              };

              lsp-completion = {
                enable = cfg.lspStyle == "lsp-mode";

                after = [ "lsp-mode" ];
                config = ''
                  (setq lsp-completion-enable-additional-text-edit nil)
                '';
              };

              lsp-diagnostics = {
                enable = cfg.lspStyle == "lsp-mode";

                after = [ "lsp-mode" ];
              };

              lsp-lens = {
                enable = cfg.lspStyle == "lsp-mode";

                command = [ "lsp-lens--enable" ];
                after = [ "lsp-mode" ];
              };

              lsp-mode = {
                enable = cfg.lspStyle == "lsp-mode";
                command = [ "lsp" ];
                after = [
                  "flycheck"
                ];
                bindLocal = {
                  lsp-mode-map = {
                    "C-c f r" = "lsp-find-references";
                    "C-r a" = "lsp-execute-code-action";
                    "C-r f" = "lsp-format-buffer";
                    "C-r g" = "lsp-format-region";
                    "C-r l" = "lsp-avy-lens";
                    "C-r r" = "lsp-rename";
                  };
                };
                init = ''
                  (setq lsp-keymap-prefix "C-r l")
                '';
                config = ''
                  (setq lsp-diagnostics-provider :flycheck
                        lsp-eldoc-render-all nil
                        lsp-enable-on-type-formatting nil
                        lsp-enable-suggest-server-download nil
                        lsp-headerline-breadcrumb-enable nil
                        lsp-lens-enable t
                        lsp-modeline-code-actions-enable nil
                        lsp-modeline-diagnostics-enable nil
                        lsp-modeline-workspace-status-enable nil)
                '';
              };

              dap-mode = {
                enable = cfg.lspStyle == "lsp-mode";
                after = [ "lsp-mode" ];
              };

              dap-ui = {
                enable = cfg.lspStyle == "lsp-mode";

                hook = [ "(dap-mode . dap-ui-mode)" ];
              };
              go-mode = {
                enable = false;
              };
              flycheck-eglot = {
                enable = true;
                after = [
                  "flycheck"
                  "eglot"
                ];
                config = "(global-flycheck-eglot-mode 1)";
              };

              ## there is also browes-at-remote
              git-link = {
                enable = true;
                command = [
                  "git-link"
                  "git-link-commit"
                  "git-link-homepage"
                ];
                config = ''
                  (setq git-link-open-in-browser t)
                '';
              };

              tree-sitter = {
                enable = true;
                config = ''
                  (global-tree-sitter-mode)
                  (add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode)
                  (setq major-mode-remap-alist
                  '((yaml-mode . yaml-ts-mode)
                  (bash-mode . bash-ts-mode)
                    (js2-mode . js-ts-mode)
                  ;;  (java-mode . java-ts-mode)
                    (json-mode . json-ts-mode)
                    ;;(kotlin-mode . kotlin-ts-mode)
                    (css-mode . css-ts-mode)
                    (python-mode . python-ts-mode)))'';
              };
              tree-sitter-langs = {
                enable = true;
                config = ''
                  (add-to-list 'tree-sitter-major-mode-language-alist '(markdown-mode . markdown))

                '';
              };
              transient-showcase = {
                enable = true;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/transient-showcase.nix {
                    inherit (pkgs) fetchFromGitHub;
                    inherit (epkgs) trivialBuild transient;
                  });
                config = ''
                  (transient-define-prefix tsc-hello ()
                   "Prefix that is minimal and uses an anonymous command suffix."
                   [("s" "call suffix"
                     (lambda ()
                       (interactive)
                       (message "Called a suffix")))])
                '';
              };

              # https://github.com/casouri/vundo
              vundo = {
                enable = true;
                defer = 1;
                command = [
                  "vundo"
                ];
              };
              copilot = {
                enable = cfg.lspStyle != "lsp-bridge" && cfg.enableCopilot;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/copilot-emacs {
                    inherit (pkgs) fetchFromGitHub nodejs;
                    inherit lib;
                    inherit (epkgs)
                      trivialBuild
                      dash
                      editorconfig
                      s
                      f
                      jsonrpc
                      ;
                  }

                  );
                hook = [ "(java-mode . copilot-mode)" ];
                bindLocal = {
                  copilot-completion-map = {
                    "<tab>" = "copilot-accept-completion";
                    "M-n" = "copilot-next-completion";
                    "M-p" = "copilot-previous-completion";
                  };
                };
              };
              lspce = {
                enable = cfg.lspStyle == "lspce";
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/lspce.nix {
                    inherit lib;
                    inherit (pkgs) fetchFromGitHub rustPlatform;
                    inherit (epkgs)
                      trivialBuild
                      f
                      yasnippet
                      markdown-mode
                      ;
                  });
                config = ''

                        (progn
                  (setq lspce-send-changes-idle-time 1)
                  (setq lspce-show-log-level-in-modeline t) ;; show log level in mode line

                  ;; You should call this first if you want lspce to write logs
                  (lspce-set-log-file "/tmp/lspce.log")

                  ;; By default, lspce will not write log out to anywhere.
                  ;; To enable logging, you can add the following line
                  ;; (lspce-enable-logging)
                  ;; You can enable/disable logging on the fly by calling `lspce-enable-logging' or `lspce-disable-logging'.

                  ;; enable lspce in particular buffers
                  ;; (add-hook 'rust-mode-hook 'lspce-mode)

                  ;; modify `lspce-server-programs' to add or change a lsp server, see document
                  ;; of `lspce-lsp-type-function' to understand how to get buffer's lsp type.
                  ;; Bellow is what I use
                  (setq lspce-server-programs `(
                                                ("python" "pylsp" "" )
                                                ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
                                              
                                                ))
                  )

                '';
              };
              yequake = {
                enable = true;
                custom = "
                (yequake-frames
                 '((\"org-capture\"
                   (buffer-fns . (yequake-org-capture))
                (width . 0.75)
                (height . 0.5)
                (alpha . 0.95)
                (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)
                           (window-system . ns)
                           ))
                           )))
                ";
              };
              consult-omni = {
                enable = true;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/consult-omni {
                    inherit (pkgs) fetchFromGitHub writeText unstableGitUpdater;
                    inherit lib;
                    inherit (epkgs)
                      melpaBuild
                      consult
                      yequake
                      elfeed
                      embark
                      browser-hist
                      consult-notes
                      ;
                  });

                config = ''
                                      ;; Load Sources Core code
                                      (require 'consult-omni-sources)

                                      (setq consult-omni-sources-modules-to-load (list 'consult-omni-apps 'consult-omni-notes))
                                      (consult-omni-sources-load-modules)
                                      ;; Load Embark Actions
                                      (require 'consult-omni-embark)
                                      (setq consult-omni-apps-paths (append (file-expand-wildcards "/Applications/Adobe*")
                                      (list "/Applications"
                                            "/Applications/Utilities/"
                                            "/System/Applications/"
                                            "/System/Applications/Utilities/"
                                            "/System/Library/CoreServices/Applications/"
                                            "~/Applications/"
                                            "~/Applications/Home Manager Apps"
                                            )))

                                      ;;; set multiple sources for consult-omni-multi command. Change these lists as needed for different interactive commands. Keep in mind that each source has to be a key in `consult-omni-sources-alist'.
                                      (setq consult-omni-multi-sources '("calc"
                                                                         ;; "File"
                                                                         ;; "Buffer"
                                                                         ;; "Bookmark"
                                                                         "Apps"
                                                                         ;; "gptel"
                                                                         ;; "Brave"
                                                                         "Dictionary"
                                                                         ;; "Google"
                                                                         "Wikipedia"
                                                                         ;; "elfeed"
                                                                         ;; "mu4e"
                                                                         ;; "buffers text search"
                                                                         "Notes Search"
                                                                         "Org Agenda"
                                                                         ;;"GitHub"
                                                                         ;; "YouTube"
                                                                         ;; "Invidious"
                                                                         ))
                                                                         ;;; Set your shorthand favorite interactive command
                                                                         (setq consult-omni-default-interactive-command #'consult-omni-multi)
                  (defun consult-omni-app-launcher ()
                    (interactive)
                    (let* (
                           (vertico-count 30)
                           (width (floor (* 0.8 (display-pixel-width))))
                           (height (floor (* 0.8 (display-pixel-height))))
                           (left  (floor (* 0.1 (display-pixel-width))))
                           (top (floor (* 0.1 (display-pixel-height))))
                           (params `((name . "demo-omni")
                                    (width . ,(cons 'text-pixels width))
                                    (height . ,(cons 'text-pixels height))
                                    (left . ,left)
                                    ;; only work for macos, also kind of needed
                                    (window-system . ns)
                                    (top . ,top)
                                    (minibuffer . only)))
                            (frame (make-frame params)))
                          (with-selected-frame frame
                            (select-frame-set-input-focus (selected-frame))
                            (unwind-protect
                                (progn (consult-omni-apps-static ".*" (propertize "  " 'face 'consult-omni-path-face))
                                       nil)
                              (progn
                                (when (frame-live-p frame) (delete-frame frame))
                                nil))
                                )))
                '';

              };
              lsp-bridge = {
                enable = cfg.lspStyle == "lsp-bridge";
                # hook = [ "( after-init . global-lsp-bridge-mode)" ];
                config = ''
                  (require 'lsp-bridge-jdtls)
                  (require 'yasnippet)
                  (yas-global-mode 1)
                  (add-hook 'direnv-envrc-mode-hook 'lsp-bridge-restart-process)
                  ;;(setopt lsp-bridge-jdtls-jvm-args  "-javaagent:${pkgs.lombok}/share/java/lombok.jar")
                  (setq lsp-bridge-enable-auto-import 't)
                '';
              };

              # We just need the package to be available since enable this manually in
              # early init.
              # https://protesilaos.com/emacs/ef-themes-pictures
              ef-themes = {
                enable = true;
                preface = ''
                   (defun my/select-light-theme ()
                       (interactive)
                       (ef-themes-select 'ef-day)
                       )
                  (defun my/select-dark-theme ()
                         (interactive)
                   (ef-themes-select 'ef-dream))
                '';
                bind = {
                  "C-c t l" = "my/select-light-theme";
                  "C-c t d" = "my/select-dark-theme";
                  "C-c t t" = "ef-themes-toggle";
                };
              };

              auto-dark = {
                enable = isDarwin;
                hook = [
                  " (after-init . auto-dark-mode) "
                  ''
                     (auto-dark-dark-mode
                    . (lambda ()
                           (ef-themes-select 'ef-dream)
                         ))
                  ''
                  ''
                              (auto-dark-light-mode
                    . (lambda ()
                      (ef-themes-select 'ef-day)
                         ))
                  ''
                ];
                extraConfig = ''

                  :custom
                  (custom-safe-themes '((ef-day) (ef-dream) (ef-winter)))
                  (auto-dark-themes '((ef-dream) (ef-day)))
                  (auto-dark-polling-interval-seconds 5)
                  (auto-dark-allow-osascript t)
                '';
              };
              # Read the lin manual: <https://protesilaos.com/emacs/lin>.
              lin = {
                enable = true;
                hook = [ " (after-init . lin-global-mode) " ];
                config = ''
                  ;; You can use this to live update the face:
                  ;;
                  ;; (customize-set-variable 'lin-face 'lin-green)
                  ;;
                  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
                  ;;
                  ;; I still prefer `setq' for consistency.
                  (setq lin-face 'lin-green)'';
              };
              # Display current time
              time = {
                enable = true;

                hook = [ "(after-init . display-time-mode)" ];
                config = ''
                  (setq display-time-format " %a %e %b, %H:%M ")
                  ;;;; Covered by `display-time-format'
                  ;; (setq display-time-24hr-format t)
                  ;; (setq display-time-day-and-date t)
                  (setq display-time-interval 60)
                  (setq display-time-default-load-average nil)
                  ;; NOTE 2022-09-21: For all those, I have implemented my own solution
                  ;; that also shows the number of new items, although it depends on
                  ;; notmuch: the `notmuch-indicator' package.
                  (setq display-time-mail-directory nil)
                  (setq display-time-mail-function nil)
                  (setq display-time-use-mail-icon nil)
                  (setq display-time-mail-string nil)
                  (setq display-time-mail-face nil)

                  ;; I don't need the load average and the mail indicator, so let this
                  ;; be simple:
                  (setq display-time-string-forms
                        '((propertize
                           (format-time-string display-time-format now)
                           'face 'display-time-date-and-time
                           'help-echo (format-time-string "%a %b %e, %Y" now))
                           " "))
                '';
              };
              all-the-icons = {
                enable = true;
              };

              yasnippet = {
                enable = true;
                diminish = [ "yas-minor-mode" ];
                bind = {
                  "C-c C-s" = "yas-insert-snippet";
                };

                command = [
                  "yas-global-mode"
                  "yas-minor-mode"
                  "yas-expand-snippet"
                ];
                hook = [
                  # Yasnippet interferes with tab completion in ansi-term.
                  "(term-mode . (lambda () (yas-minor-mode -1)))"
                  "(prog-mode . yas-minor-mode-on)"
                ];
                config = "(yas-global-mode 1)";
              };

              consult-yasnippet = {
                enable = true;
                after = [
                  "consult"
                  "yasnippet"
                ];
                command = [ "consult-yasnippet" ];
              };

              auto-yasnippet = {
                enable = true;
                after = [ "yasnippet" ];
                bind = {
                  "C-c y a" = "aya-create";
                  "C-c y e" = "aya-expand";
                  "C-c y o" = "aya-open-line";
                };
              };

              consult-dir = {
                enable = true;
                bind = {
                  "M-g d" = "consult-dir";
                };

                bindLocal = {
                  minibuffer-local-filename-completion-map = {
                    "M-g d" = "consult-dir";
                    "M-s f" = "consult-dir-jump-file";
                  };
                };
              };

              consult-dir-vertico = {
                enable = true;
                noRequire = true;
                after = [
                  "consult-dir"
                  "vertico"
                ];
                # :defines (vertico-map)
                # :bind (:map vertico-map
                #             ("C-x C-j" . consult-dir)
                #             ("M-g d"   . consult-dir)
                #             ("M-s f"   . consult-dir-jump-file)))
              };
              kind-icon = {
                enable = true;
                after = [ "corfu" ];
                # (setq kind-icon-blend-background t)
                # (setq kind-icon-default-face 'corfu-default)
                config = ''
                  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter )
                '';
              };

              copy-as-format = {
                enable = false;
                command = [
                  "copy-as-format"
                  "copy-as-format-asciidoc"
                  "copy-as-format-bitbucket"
                  "copy-as-format-disqus"
                  "copy-as-format-github"
                  "copy-as-format-gitlab"
                  "copy-as-format-hipchat"
                  "copy-as-format-html"
                  "copy-as-format-jira"
                  "copy-as-format-markdown"
                  "copy-as-format-mediawiki"
                  "copy-as-format-org-mode"
                  "copy-as-format-pod"
                  "copy-as-format-rst"
                  "copy-as-format-slack"
                ];
              };

              css-ts-mode.enable = true;
              grep = {
                enable = true;
              };
              wgrep = {
                enable = true;
                custom = ''
                  (wgrep-auto-save-buffer t)
                '';
              };
              delsel = {
                enable = true;
                hook = [
                  "(after-init . delete-selection-mode)"
                ];
              };
              deadgrep = {
                enable = true;
                command = [ "deadgrep" ];
              };

              aggressive-indent = {
                enable = false;
                diminish = [ "aggressive-ident-mode" ];
                hook = [
                  # :diminish
                  "(emacs-lisp-mode . aggressive-indent-mode)"
                ];
              };
              # Enable Electric Indent mode to do automatic indentation on RET.
              electric = {
                enable = true;
                command = [ "electric-indent-local-mode" ];
                hook = [
                  "(prog-mode . electric-indent-mode)"
                ];
                config = ''
                  (electric-pair-mode -1)
                  (electric-quote-mode -1)
                  (electric-indent-mode -1)
                  (electric-indent-local-mode -1)
                '';
              };

              cape = {
                enable = true;
                demand = true;
                # Alternative prefix keys: C-c p, M-p, M-+, ...
                bind = {
                  "C-c p p" = "completion-at-point";
                  "C-c p t" = "complete-tag";
                  "C-c p h" = "cape-history";
                };
                init = ''
                  ;; Add `completion-at-point-functions', used by `completion-at-point'.
                  (add-all-to-list 'completion-at-point-functions
                     #'cape-dabbrev
                     #'cape-file
                     #'cape-abbrev)
                '';
              };

              ws-butler = {
                enable = false;
                config = "(ws-butler-global-mode)";
              };
              vterm = {
                enable = true;
                defer = true;
                command = [
                  "vterm"
                  "vterm-other-window"
                ];
                config = ''
                  (setq vterm-kill-buffer-on-exit t
                        vterm-max-scrollback 10000)
                '';
              };
              eat = {
                enable = true;
              };
              multi-vterm = {
                enable = true;
                command = [
                  "multi-vterm"
                  "multi-vterm-project"
                ];
                defer = true;
              };

              wm = {
                enable = false;
                package =
                  epkgs:
                  epkgs.trivialBuild rec {
                    pname = "wm";
                    version = "0.0.1";
                    src = ./packages/wm.el;
                    propagatedUserEnvPkgs = [
                      epkgs.hydra
                      epkgs.ace-window
                    ];
                    buildInputs = propagatedUserEnvPkgs;
                  };
              };

              alert = {
                enable = true;
                extraConfig = ''
                  :autoload (alert alert-add-rule)
                '';
                custom = ''
                  ;; (alert-default-style 'fringe)
                  (alert-default-style 'osx-notifier)
                  (alert-notifier-command
                   "${pkgs.terminal-notifier}/bin/terminal-notifier")
                '';
                extraPackages = [
                  pkgs.terminal-notifier
                ];
              };

              spacious-padding = {
                enable = true;
                custom = ''
                  (spacious-padding-widths
                   '( :internal-border-width 15
                      :header-line-width 4
                      :mode-line-width 3                ; half the default
                      :tab-width 4
                      :right-divider-width 15           ; half the default
                      :scroll-bar-width 8))
                '';
                config = "(spacious-padding-mode 1)";
              };

              toggle-term = {
                enable = false;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "toggle-term";
                    version = "0.0.2";
                    src = pkgs.fetchFromGitHub {
                      owner = "justinlime";
                      repo = "toggle-term.el";
                      rev = "d17596b8ed52e6a2f0c7f6754ee6fa233d28c146";
                      # sha256 = lib.fakeSha256;
                      sha256 = "sha256-haiNWztItstQPT/NPxzioYpLgf9hJcihaxeDB64ZNxA=";
                    };
                    preferLocalBuild = true;
                    allowSubstitutes = false;
                  };
                config = ''
                  (setq toggle-term-size 25)
                  (setq toggle-term-switch-upon-toggle t)
                '';
                bind = {
                  "M-o f" = "toggle-term-find";
                  "M-o t" = "toggle-term-term";
                  "M-o s" = "toggle-term-shell";
                  "M-o e" = "toggle-term-eshell";
                  "M-o i" = "toggle-term-ielm";
                  "M-o v" = "toggle-term-vterm";
                  "M-o o" = "toggle-term-toggle";
                };
              };

              gptel = {
                enable = true;
              };

              mcp = {
                enable = false;
                after = [ "gptel" ];
                extraConfig = ''
                              
                  :custom (mcp-hub-servers
                           `(("filesystem" . (:command "mcp-server-filesystem" :args ("~/Users/yuanwang")))
                             ;;("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
                             ;;("qdrant" . (:url "http://localhost:8000/sse"))
                             ;; ("graphlit" . (
                             ;;                :command "npx"
                             ;;                :args ("-y" "graphlit-mcp-server")
                             ;;                :env (
                             ;;                      :GRAPHLIT_ORGANIZATION_ID "your-organization-id"
                             ;;                      :GRAPHLIT_ENVIRONMENT_ID "your-environment-id"
                             ;;                      :GRAPHLIT_JWT_SECRET "your-jwt-secret")))
                                                  )
                                                  )
                  :config (require 'mcp-hub)
                  :hook (after-init . mcp-hub-start-all-server)
                '';
              };

              eaf = {
                # enable = true;
                extraConfig = ''
                                    :custom
                  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
                  (eaf-browser-continue-where-left-off t)
                  (eaf-browser-enable-adblocker t)
                  (browse-url-browser-function 'eaf-open-browser)
                  :config
                  (defalias 'browse-web #'eaf-open-browser)
                  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
                  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
                  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
                  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
                '';
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/eaf {
                    elpa2nix = "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/elpa2nix.el";
                    melpa2nix = "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/melpa2nix.el";

                    inherit (epkgs) melpaBuild;
                    inherit (epkgs.melpaPackages)
                      ctable
                      deferred
                      epc
                      s
                      ;
                  });

              };

              eaf-browser = {
                enable = false;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/eaf-browser.nix {
                    inherit (epkgs) melpaBuild;
                  });
              };

              eaf-pdf-viewer = {
                enable = true;
                package =
                  epkgs:
                  (pkgs.callPackage ./packages/eaf-pdf-viewer.nix {
                    elpa2nix = "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/elpa2nix.el";
                    melpa2nix = "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/melpa2nix.el";
                    inherit (epkgs) melpaBuild;
                  });
              };

            };
          };

          home = {
            file.".emacs.d/snippets".source =
              hm.config.lib.file.mkOutOfStoreSymlink "${config.my.homeDirectory}/workspaces/nix-home/modules/editor/emacs/snippets";
            packages = with pkgs; [
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
              BasedOnStyles = alex, google, Microsoft, Joblint, proselint, write-good
            '';
            # file.".emacs.d".source = emacsConfigPath;
          };
          # not use home-manager programs.emacs due to it wraps
          # emacsWithPackages again
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
