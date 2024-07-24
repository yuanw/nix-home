# most of this is stealed from rycee emacs module
# https://git.sr.ht/~rycee/configurations/tree/master/item/user/emacs.nix
# other codes stealed from
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

    lspStyle = mkOption {
      type = types.enum [ "eglot" "lsp-bridge" "lspce" ];
      default = "eglot";
    };

    enableService = mkOption {
      type = types.bool;
      default = false;
    };

    enableCopilot = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {

      # services.emacs = {
      #   enable = cfg.enableService;
      #   additionalPath = [ "${config.my.homeDirectory}" ];
      #   package = config.home-manager.users.${config.my.username}.programs.emacs.finalPackage;
      # };

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
            [
              epkgs.treesit-grammars.with-all-grammars
              (epkgs.trivialBuild {
                pname = "prot-common";
                version = "0.0.1";
                src = ./packages/prot-common.el;

              }
              )

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
              ;; (push '(menu-bar-lines . 0) default-frame-alist)
              (push '(tool-bar-lines . nil) default-frame-alist)
              (push '(vertical-scroll-bars . nil) default-frame-alist)
              ;; no title bar
              (add-to-list 'default-frame-alist '(undecorated-round . t))
              ;; Set up fonts early.
              ;;--------------------

              (set-face-attribute 'default nil
               :font "PragmataPro"
              :height 160
              )
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

              ;; Enable some features that are disabled by default.
              (put 'narrow-to-region 'disabled nil)
              (put 'upcase-region 'disabled nil)
              (put 'downcase-region 'disabled nil)
              ;; Typically, I only want spaces when pressing the TAB key. I also
              ;; want 4 of them.
              (setq-default indent-tabs-mode nil
                            tab-width 4
                            c-basic-offset 4)
              ;; Trailing white space are banned!
              ;;(setq-default show-trailing-whitespace t)

              ;; Use one space to end sentences.
              (setq sentence-end-double-space nil)

              ;;use UTF-8.
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

              ;; https://www.emacswiki.org/emacs/RecursiveEdit
              ;;(setq enable-recursive-minibuffer t)
              (defun stop-using-minibuffer ()
              "kill the minibuffer"
              (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
              (abort-recursive-edit)))

              (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)
              ;; Enable indentation+completion using the TAB key.
              ;; `completion-at-point' is often bound to M-TAB.
              (setq tab-always-indent 'complete)


              ;; Only do candidate cycling if there are very few candidates.
              (setq completion-cycle-threshold 3)


            '';

            postlude = ''
              ;; Minimising & quitting Emacs way too many times without wanting to.
              (global-unset-key "\C-x\C-c")
              ;; add here seems actully does the trick
              (keycast-mode-line-mode)
            '';

            usePackage = {
              # pratice emacs-fu
              disable-mouse = {
                enable = true;
                config = ''
                  (global-disable-mouse-mode)
                '';
              };
              exec-path-from-shell = {
                enable = false;
                extraConfig = ":when (daemonp)";
                config = "(exec-path-from-shell-initialize)";
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

              god-mode = {
                enable = true;
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
                package = epkgs:
                  epkgs.trivialBuild {
                    pname = "prot-modeline";
                    version = "0.0.1";
                    src = ./packages/prot-modeline.el;
                    packageRequires = [
                      (
                        epkgs.trivialBuild {
                          pname = "prot-common";
                          version = "0.0.1";
                          src = ./packages/prot-common.el;
                        }
                      )

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
                                       (with-eval-after-load 'which-key
                                       (which-key-enable-god-mode-support)
                                       )
                  (with-eval-after-load 'god-mode
                      (defun my-god-mode-update-mode-line ()
                       (cond
                        (god-local-mode
                         (set-face-attribute 'mode-line nil
                                             :foreground "#604000"
                                             :background "#fff29a")
                         (set-face-attribute 'mode-line-inactive nil
                                             :foreground "#3f3000"
                                             :background "#fff3da"))
                        (t
                         (set-face-attribute 'mode-line nil
                     			:foreground "#0a0a0a"
                     			:background "#d7d7d7")
                         (set-face-attribute 'mode-line-inactive nil
                     			:foreground "#404148"
                     			:background "#efefef"))))
                           (my-god-mode-update-mode-line)
                           (add-hook 'post-command-hook #'my-god-mode-update-mode-line))

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
                hook = [ "(dired-mode . auto-revert-mode)" ];
                config = ''
                  (setq auto-revert-use-notfiy nil)
                  (global-auto-revert-mode t)
                '';
              };

              auto-save = {
                enable = true;
                package = epkgs:
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
              # operate on whole line, when no region is active
              # https://github.com/purcell/whole-line-or-region/tree/master
              whole-line-or-region = {
                enable = true;
              };
              expand-region = {
                enable = true;
                bind = {
                  "C-z" = "er/expand-region";
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
                command = [
                  "recentf-mode"
                  "recentf-add-file"
                  "recentf-apply-filename-handlers"
                ];
                config = ''
                  (setq recentf-save-file (locate-user-emacs-file "recentf")
                        recentf-max-menu-items 20
                        recentf-max-saved-items 500
                        recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"))

                  ;; Save the file list every 10 minutes.
                  (run-at-time nil (* 10 60) 'recentf-save-list)
                  (recentf-mode)
                '';
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
                command = [ "dired" "dired-jump" ];
                config = ''
                  (put 'dired-find-alternate-file 'disabled nil)

                  ;; Be smart about choosing file targets.
                  (setq dired-dwim-target t)

                  ;; Use the system trash can.
                  (setq delete-by-moving-to-trash t)
                  (setq dired-listing-switches "-alvh --group-directories-first")
                '';
              };

              wdired = {
                enable = true;
                bindLocal = {
                  dired-mode-map = { "C-c C-w" = "wdired-change-to-wdired-mode"; };
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
                bindLocal.dired-mode-map = { "." = "dired-omit-mode"; };
                config = ''
                  (setq dired-omit-verbose nil
                        dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
                '';
              };

              dired-subtree = {
                enable = true;
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
              dot-mode = {
                enable = true;
                command = [ "dot-mode" "dot-mode-execute" ];
                diminish = [ "dot-mode" ];
                bind = {
                  "C-." = "dot-mode-execute";
                };
                config = ''
                  (global-dot-mode)
                '';
              };

              free-keys = {
                enable = true;
                defer = 1;
                command = [ "free-keys" ];
              };

              orderless = {
                enable = true;
                config = ''
                  (setq completion-styles '( basic substring orderless))
                  (setq read-file-name-completion-ignore-case t)
                  (setq completion-category-overrides
                   '((file (styles basic partial-completion))))
                '';
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
                  (global-corfu-mode)
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
                   ("M-s f" . consult-find)                  ;; Alternative: consult-fd
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
                command = [ "consult-xref" ];
                config = ''
                  (setq xref-show-definitions-function #'consult-xref
                        xref-show-xrefs-function #'consult-xref)
                '';
              };
              embark-consult = {
                enable = true;
                after = [ "consult" "embark" ];
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

              hydra = {
                enable = true;
              };
              org = {
                enable = true;
                config = ''
                                   (setq org-directory  "~/org/")
                  (setq org-agenda-files (append
                                           (file-expand-wildcards (concat org-directory "*.org"))
                                           (file-expand-wildcards (concat org-directory "agenda/*.org"))
                                           (file-expand-wildcards (concat org-directory "projects/*.org"))))
                  (setq  org-default-notes-file (concat org-directory "agenda/inbox.org"))
                '';
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
                after = [ "org" ];
                hook = [
                  "(dired-mode-hook .org-download-enable)"
                ];
              };
              org-re-reveal = {
                enable = false;
                after = [ "org" ];
                config = ''
                  (setq org-re-reveal-root "${pkgs.reveal-js}/share")
                '';
              };
              dslide = {
                enable = true;
                package = epkgs:
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
                config = ''
                  (setq denote-directory "~/org/denote/")
                  (setq denote-templates
                        `((report . "* Some heading\n\n* Another heading")
                          (journal . ,(concat "* Tasks todo"
                                           "\n\n"
                                           "* TIL"
                                           "\n\n"))))

                    (use-package denote-org-extras)
                    (use-package denote-rename-buffer)
                    (use-package denote-journal-extras)
                    (denote-rename-buffer-mode)

                    (with-eval-after-load 'org-capture
                      (add-to-list 'org-capture-templates
                                   '("n" "New note (with Denote)" plain
                                     (file denote-last-path)
                                     #'denote-org-capture
                                     :no-save t
                                     :immediate-finish nil
                                     :kill-buffer t
                                     :jump-to-captured t)))
                '';
              };
              consult-denote = {
                enable = true;
                package = epkgs:
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
                bind = { "C-x g" = "magit-status"; };
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
                defer = true;
                hook = [ "(emacs-startup . global-jinx-mode)" ];
                bind = {
                  "M-$" = "jinx-correct";
                  "C-M-$" = "jinx-languages ";
                };
              };

              yaml-ts-mode = {
                enable = true;
              };
              nix-mode = {
                enable = true;
                hook = [
                  "(nix-mode . subword-mode) "
                ];
                config = ''
                  (setq nix-indent-function 'nix-indent-line) '';
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
                config = ''
                                    (setq eglot-autoshutdown t)
                                    (add-to-list 'eglot-server-programs
                                    '((java-mode java-ts-mode) .

                                    ("jdtls"
                                    "--jvm-arg=-javaagent:${pkgs.lombok}/share/java/lombok.jar"




                                      ))
                                    ((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                                    . ("${pkgs.gopls}/bin/gopls"))
                                    )
                  (add-hook 'java-mode-hook #'eglot-ensure)
                  (add-hook 'java-ts-mode-hook #'eglot-ensure)
                '';
              };

              eglot-booster = {
                enable = cfg.lspStyle == "eglot";
                package = epkgs:
                  epkgs.trivialBuild {
                    pname = "eglot-booster";
                    version = "0.0.1";
                    src = pkgs.fetchFromGitHub {
                      owner = "jdtsmith";
                      repo = "eglot-booster";
                      rev = "e19dd7ea81bada84c66e8bdd121408d9c0761fe6";
                      sha256 = "sha256-vF34ZoUUj8RENyH9OeKGSPk34G6KXZhEZozQKEcRNhs=";
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
              go-mode = {
                enable = true;
                config = ''
                '';
              };
              flycheck-eglot = {
                enable = true;
                after = [ "flycheck" "eglot" ];
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
                package = epkgs:
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
                package = epkgs: (
                  pkgs.callPackage ./packages/copilot-emacs {
                    inherit (pkgs) fetchFromGitHub nodejs;
                    inherit lib;
                    inherit (epkgs) trivialBuild dash editorconfig s f jsonrpc;
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
                package = epkgs:
                  (pkgs.callPackage ./packages/lspce.nix {
                    inherit lib;
                    inherit (pkgs) fetchFromGitHub rustPlatform;
                    inherit (epkgs) trivialBuild f yasnippet markdown-mode
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
              lsp-bridge = {
                enable = cfg.lspStyle == "lsp-bridge";
                package = epkgs: (
                  pkgs.callPackage ./packages/lsp-bridge {
                    inherit (pkgs) fetchFromGitHub substituteAll writeText python3 unstableGitUpdater;
                    inherit lib;
                    inherit (epkgs) melpaBuild markdown-mode yasnippet;
                  }
                );
                hook = [ "(java-mode . lsp-bridge-mode)" ];
                init = ''
                  (require 'lsp-bridge-jdtls)
                '';
                config = ''
                  (require 'yasnippet)
                  (yas-global-mode 1)

                  (setq acm-enable-copilot true)
                  (setq lsp-bridge-jdtls-jvm-args  (list (concat "-javaagent:" (getenv "LOMBOK_DIR") "/lombok.jar")))
                  (setq lsp-bridge-enable-auto-import t)
                  (global-lsp-bridge-mode)
                '';
              };
              # https://protesilaos.com/emacs/ef-themes-pictures
              ef-themes = {
                enable = true;
                defer = true;
                earlyInit = ''
                  ;; Set color theme in early init to avoid flashing during start.
                  (require 'ef-themes)
                  (load-theme 'ef-summer :no-confirm)
                  (setq ef-themes-to-toggle '(ef-summer ef-winter))
                '';
              };
              all-the-icons = {
                enable = true;
              };

              yasnippet = {
                enable = true;
                diminish = [ "yas-minor-mode" ];
                command = [ "yas-global-mode" "yas-minor-mode" "yas-expand-snippet" ];
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
                enable = true;
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
              wgrep = {
                enable = true;
              };
              deadgrep = {
                enable = true;
                command = [ "deadgrep" ];
              };

              # Enable Electric Indent mode to do automatic indentation on RET.
              electric = {
                enable = true;
                command = [ "electric-indent-local-mode" ];
                hook = [
                  "(prog-mode . electric-indent-mode)"

                  # Disable for some modes.
                  "(purescript-mode . (lambda () (electric-indent-local-mode -1)))"
                ];
              };
              cape = {
                enable = true;
                # Alternative prefix keys: C-c p, M-p, M-+, ...
                bind = {
                  "C-c p p" = "completion-at-point";
                  "C-c p t" = "complete-tag";
                  "C-c p h" = "cape-history";
                };
                init = ''
                  ;; Add to the global default value of `completion-at-point-functions' which is
                  ;; used by `completion-at-point'.  The order of the functions matters, the
                  ;; first function returning a result wins.  Note that the list of buffer-local
                  ;; completion functions takes precedence over the global list.
                  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
                  (add-to-list 'completion-at-point-functions #'cape-file)
                  (add-to-list 'completion-at-point-functions #'cape-abbrev)
                  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-block)
                  (add-to-list 'completion-at-point-functions #'cape-history)
                  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
                  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
                  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
                  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
                  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
                  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
                  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
                  ;;(add-to-list 'completion-at-point-functions #'cape-line)
                '';
              };

              ws-butler = {
                enable = true;
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
                enable = true;
                package = epkgs:
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


              toggle-term = {
                enable = false;
                package = epkgs:
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
                stylesPath = pkgs.linkFarm "
            vale-styles "
                  valeStyles;
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
              ASPELL_CONF = "dict-dir ${aspell}/lib/aspell";
            };

          };
        };



    }
    {
      fonts.packages = [ pkgs.emacs-all-the-icons-fonts ];
    }


    (if (builtins.hasAttr "launchd" options) then {

      launchd.user.agents.emacs.serviceConfig = {
        StandardOutPath = "/tmp/emacs.log";
        StandardErrorPath = "/tmp/emacs.log";
      };
    } else
      { })
  ]);
}
