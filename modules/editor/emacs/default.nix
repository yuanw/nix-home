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
        ./patches/round-undecorated-frame.patch
        "${inputs.emacs-plus}/patches/emacs-31/system-appearance.patch"
      ])
      ++ prev.patches;
  });

  valeStyles = [
    {
      name = "alex";
      path = "${inputs.vale-alex}/alex";
    }
    {
      name = "Google";
      path = "${inputs.vale-Google}/Google";
    }
    {
      name = "Microsoft";
      path = "${inputs.vale-Microsoft}/Microsoft";
    }
    {
      name = "Joblint";
      path = "${inputs.vale-Joblint}/Joblint";
    }
    {
      name = "proselint";
      path = "${inputs.vale-proselint}/proselint";
    }
    {
      name = "write-good";
      path = "${inputs.vale-write-good}/write-good";
    }
  ];
  emacsPackage = config.home-manager.users.${config.my.username}.programs.emacs.finalPackage;
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
            texlive.combined.scheme-medium
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
        { pkgs, ... }:
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
              (let ((mono-spaced-font "PragmataPro Mono Liga")
                    (proportionately-spaced-font "PragmataPro"))
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
              (setq use-short-answers t)
              (setq use-dialog-box nil)

              ;; Don't want to move based on visual line.
              (setq line-move-visual nil)

                 ;; TODO maybe should re-configure this
                 ;; Stop creating backup and autosave files.
                 (setq make-backup-files nil
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
                 (global-hl-line-mode 1)

                 ;; Avoid noisy bell.
                 (setq visible-bell t)

                 ;; Enable indentation+completion using the TAB key.
                 ;; `completion-at-point' is often bound to M-TAB.
                 (setq tab-always-indent 'complete)
                 (setq xref-search-program 'ripgrep)


                 (defun prot/keyboard-quit-dwim ()
                      "Do-What-I-Mean behaviour for a general `keyboard-quit'.
                      The generic `keyboard-quit' does not do the expected thing when
                      the minibuffer is open.  Whereas we want it to close the
                      minibuffer, even without explicitly focusing it.
                      The DWIM behaviour of this command is as follows:

                      - When the region is active, disable it.
                      - When a minibuffer is open, but not focused, close the minibuffer.
                      - When the Completions buffer is selected, close it.
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
            '';

            postlude = ''
              ;; Minimising & quitting Emacs way too many times without wanting to.
              (keycast-header-line-mode)

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
              eyebrowse = {
                enable = true;
                config = "(eyebrowse-mode t)";
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
                          (set-face-attribute 'prot-modeline-indicator-button nil :box t)
                        (set-face-attribute 'prot-modeline-indicator-button nil :box 'unspecified)))

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
                #   config = ''
                #     ;;; this actually make dired buffer refresh
                #     (add-hook 'dired-mode-hook 'auto-revert-mode)
                #     (global-auto-revert-mode t)
                #   '';
              };

              ultra-scroll = {
                enable = true;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "ultra-scroll";
                    version = "0.0.1";
                    src = pkgs.fetchFromGitHub {
                      owner = "jdtsmith";
                      repo = "ultra-scroll";
                      rev = "f5a6e8e8c411d58860f0dec2dccefc689924a8cb";
                      # sha256 = lib.fakeSha256;
                      sha256 = "sha256-32jXz4ek3CsW8JKZ5W1xOv6+UufAMcfZenjoqWc9U4g=";
                    };
                  };
                init = ''
                  (setq scroll-conservatively 101
                  scroll-margin 0 )
                '';
                config = "(ultra-scroll-mode 1)";
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
                init = "(savehist-mode)";
                config = ''
                  (setq history-delete-duplicates t
                        history-length 1000)
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

              evil = {
                enable = true;
                command = [ "evil-mode" ];
              };

              vertico = {
                enable = true;
                config = ''
                   (vertico-mode)
                   ;; Different scroll margin
                   (setq vertico-scroll-margin 0)

                   ;; Show more candidates
                   ;; (setq vertico-count 20)

                   ;; Grow and shrink the Vertico minibuffer
                   (setq vertico-resize t)

                   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
                   (setq vertico-cycle t)

                   (use-package vertico-directory
                   :bind (:map vertico-map
                               ("RET" . vertico-directory-enter)
                               ("DEL" . vertico-directory-delete-char)
                               ("M-DEL" . vertico-directory-delete-word))
                   ;; Tidy shadowed file names
                   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)

                   )

                  (use-package vertico-quick
                    :demand t
                   :after vertico
                   :bind (
                           :map vertico-map
                           ("M-q" . vertico-quick-insert)
                           ("C-q" . vertico-quick-exit))
                    :init
                    (progn
                      (setq vertico-quick1 "haio")
                      (setq vertico-quick2 "luy")))
                '';
              };

              vertico-repeat = {
                enable = true;
                demand = true;
                bind = {
                  "C-c . ." = "vertico-repeat";
                };
                config = "(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)";
              };

              vertico-multiform = {
                enable = true;
                demand = true;
                config = ''
                  (require 'vertico-grid)
                  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
                  (vertico-multiform-mode)
                '';
              };

              marginalia = {
                enable = true;
                config = "(marginalia-mode)";
              };

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
                  ;; ("M-s c" . consult-locate)
                  ;; ("M-s g" . consult-grep)
                  ;; ("M-s G" . consult-git-grep)
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

                config = ''
                               (setq consult-narrow-key "<")
                               (require 'consult-xref)
                               (require 'consult-register)
                                (consult-customize
                  consult-theme
                  :preview-key '(:debounce 0.2 any)
                  consult-ripgrep
                  consult-git-grep
                  consult-grep
                  consult-bookmark
                  consult-recent-file

                  consult--source-bookmark
                  consult--source-file-register
                  consult--source-recent-file
                  consult--source-project-recent-file
                  :preview-key '(:debounce 0.4 any))

                '';
              };

              consult-xref = {
                enable = false;
                command = [ "consult-xref" ];
                config = ''
                  (setq xref-show-definitions-function #'consult-xref
                        xref-show-xrefs-function #'consult-xref)
                '';
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
                    (agda . t)
                    (racket . t)
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
                      substituteAll
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
                      substituteAll
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
                enable = true;
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
                enable = true;
                hook = [
                  "(org-mode . org-pdftools-setup-link)"
                ];
              };
              org-review = {
                enable = true;
              };
              dslide = {
                enable = true;
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
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "denote";
                    version = "3.2.0-dev";
                    src = pkgs.fetchFromGitHub {
                      owner = "protesilaos";
                      repo = "denote";
                      rev = "5651dc77374bddf4b0aca241599fc15f994aa18c";
                      #sha256 = lib.fakeSha256;
                      sha256 = "sha256-DUW50RSnevwB7zQvKIXjyDDnMX6h1+NmYwB1J/MpXbg=";
                    };
                    preferLocalBuild = true;
                    allowSubstitutes = false;
                  };

                config = ''
                            (setq denote-directory "~/org/denote/")
                            (setq denote-templates
                                  `((report . "* Some heading\n\n* Another heading")
                                    (journal . ,(concat "* Tasks todo"
                                                     "\n\n"
                                                     "* TIL"
                                                     "\n\n"))))
                            (use-package denote-sequence)
                            (use-package denote-org-extras)
                            (use-package denote-rename-buffer)
                            (use-package denote-journal-extras)
                            (denote-rename-buffer-mode)

                            (with-eval-after-load 'org-capture
                  (push '("n" "New note (With Denote)" plain (file denote-last-path) #'denote-org-capture :no-save t
                          :immediate-finish nil :kill-buffer t :jump-to-captured t ) org-capture-templates)
                  (
                   push '("d" "daily note" item (function denote-journal-extras-new-or-existing-entry) "- %U %?" :prepend t) org-capture-templates)
                  )
                '';
              };
              consult-denote = {
                enable = false;
                package =
                  epkgs:
                  epkgs.trivialBuild {
                    pname = "consult-denote";
                    version = "0.0.1";
                    src = pkgs.fetchFromGitHub {
                      owner = "protesilaos";
                      repo = "consult-denote";
                      rev = "b477a6ec64a148c186e7114135873e975b05074f";
                      # sha256 = lib.fakeSha256;
                      sha256 = "sha256-34xSYhCO10riTGBWr2LDCznHdNVoIQtZOtiIkzdJVEc=";
                    };
                    # elisp dependencies
                    packageRequires = [
                      epkgs.denote
                      epkgs.consult
                    ];
                    preferLocalBuild = true;
                    allowSubstitutes = false;
                  };
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
                            help-mode)
                        popper-group-function #'popper-group-by-project)
                  (popper-mode)
                '';
              };
              direnv = {
                enable = true;
                config = ''
                  (direnv-mode)
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

              agda2-mode = {
                enable = config.modules.dev.agda.enable;
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
                config = ''
                  (setq eglot-autoshutdown t)
                  (add-to-list 'eglot-server-programs
                            '((java-mode java-ts-mode) . ("jdtls"
                                 "--jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar"))
                             ((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                                    . ("${pkgs.gopls}/bin/gop
                                    ls"))
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
                    version = "0.1.0";
                    src = pkgs.fetchFromGitHub {
                      owner = "jdtsmith";
                      repo = "eglot-booster";
                      rev = "e6daa6bcaf4aceee29c8a5a949b43eb1b89900ed";
                      #sha256 = lib.fakeSha256;
                      sha256 = "sha256-PLfaXELkdX5NZcSmR1s/kgmU16ODF8bn56nfTh9g6bs=";
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
                enable = true;

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
                    (java-mode . java-ts-mode)
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
                                                ("java" "jdtls" "--jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar")
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
                                            "~/Applications/"
                                            "~/.nix-profile/Applications/"
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
                  (setq lsp-bridge-jdtls-jvm-args  "-javaagent:${pkgs.lombok}/share/java/lombok.jar")
                  (setq lsp-bridge-enable-auto-import 't)
                '';
              };
              # https://protesilaos.com/emacs/ef-themes-pictures
              ef-themes = {
                enable = true;
                demand = true;
                custom = ''
                  (ef-themes-variable-pitch-ui t)
                  (ef-themes-mixed-fonts t)
                  (ef-themes-headings ; read the manual's entry of the doc string
                   '((0 . (variable-pitch light 1.9))
                     (1 . (variable-pitch light 1.8))
                     (2 . (variable-pitch regular 1.7))
                     (3 . (variable-pitch regular 1.6))
                     (4 . (variable-pitch regular 1.5))
                     (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
                     (6 . (variable-pitch 1.3))
                     (7 . (variable-pitch 1.2))
                     (agenda-date . (semilight 1.5))
                     (agenda-structure . (variable-pitch light 1.9))
                    (t . (variable-pitch 1.1))))
                '';
                config = ''
                  ;; Set color theme in early init to avoid flashing during start.

                  (load-theme 'ef-spring :no-confirm)
                  (setq ef-themes-to-toggle '(ef-spring ef-dream))
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
                command = [ "consult-yasnippet" ];
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
                init = ''
                  (use-package vertico
                    :bind (:map vertico-map
                           ("M-g d" . consult-dir)
                           ("M-s f" . consult-dir-jump-file)
                           ))
                   :config
                  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
                  (setq consult-dir-shadow-filenames nil)
                '';
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
                  #                  :diminish
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
            };
          };

          home = {
            file.".emacs.d/snippets".source = ./snippets;
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
              vale
            ];
            # file.".emacs.d".source = emacsConfigPath;
            file.".vale.ini".text =
              let
                stylesPath = pkgs.linkFarm "
            vale-styles " valeStyles;
                basedOnStyles = concatStringsSep ", " (zipAttrsWithNames [ "name" ] (_: v: v) valeStyles).name;
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
              ASPELL_CONF = "dict-dir ${aspell}/lib/aspell";
            };

          };
        };
    }
  ]);
}
