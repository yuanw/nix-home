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
          callPackage ./lsp-bridge {
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
            [
              epkgs.treesit-grammars.with-all-grammars
              cape
              corfu
              consult
              consult-dir
              consult-eglot
              consult-flycheck
              consult-git-log-grep
              consult-yasnippet
              consult-org-roam
              kind-icon
              vertico
              yasnippet
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
              (set-face-attribute 'default nil
              :font "PragmataPro Liga"
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

              ;; Enable indentation+completion using the TAB key.
              ;; `completion-at-point' is often bound to M-TAB.
              (setq tab-always-indent 'complete)

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
              (use-package corfu
                ;; Optional customizations
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

                ;; Enable Corfu only for certain modes.
                ;; :hook ((prog-mode . corfu-mode)
                ;;        (shell-mode . corfu-mode)
                ;;        (eshell-mode . corfu-mode))

                ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
                ;; be used globally (M-/).  See also the customization variable
                ;; `global-corfu-modes' to exclude certain modes.
                :config
                (global-corfu-mode))

              ;; A few more useful configurations...
              (use-package emacs
                :init
                ;; TAB cycle if there are only few candidates
                (setq completion-cycle-threshold 3)

                ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
                ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
                ;; (setq read-extended-command-predicate
                ;;       #'command-completion-default-include-p)

                ;; Enable indentation+completion using the TAB key.
                ;; `completion-at-point' is often bound to M-TAB.
                (setq tab-always-indent 'complete))

              ;; Add extensions
              (use-package cape

                ;; Bind dedicated completion commands
                ;; Alternative prefix keys: C-c p, M-p, M-+, ...
                :bind (("C-c p p" . completion-at-point) ;; capf
                       ("C-c p t" . complete-tag)        ;; etags
                       ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
                       ("C-c p h" . cape-history)
                       ("C-c p f" . cape-file)
                       ("C-c p k" . cape-keyword)
                       ("C-c p s" . cape-elisp-symbol)
                       ("C-c p e" . cape-elisp-block)
                       ("C-c p a" . cape-abbrev)
                       ("C-c p l" . cape-line)
                       ("C-c p w" . cape-dict)
                       ("C-c p :" . cape-emoji)
                       ("C-c p \\" . cape-tex)
                       ("C-c p _" . cape-tex)
                       ("C-c p ^" . cape-tex)
                       ("C-c p &" . cape-sgml)
                       ("C-c p r" . cape-rfc1345))
                :init
                ;; Add to the global default value of `completion-at-point-functions' which is
                ;; used by `completion-at-point'.  The order of the functions matters, the
                ;; first function returning a result wins.  Note that the list of buffer-local
                ;; completion functions takes precedence over the global list.
                (add-to-list 'completion-at-point-functions #'cape-dabbrev)
                (add-to-list 'completion-at-point-functions #'cape-file)
                (add-to-list 'completion-at-point-functions #'cape-elisp-block)
                ;;(add-to-list 'completion-at-point-functions #'cape-history)
                ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
                ;;(add-to-list 'completion-at-point-functions #'cape-tex)
                ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
                ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
                ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
                ;;(add-to-list 'completion-at-point-functions #'cape-dict)
                ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
                ;;(add-to-list 'completion-at-point-functions #'cape-line)
              )

              (use-package kind-icon
              :after corfu
              :custom
              (kind-icon-blend-background t)
              (kind-icon-default-face 'corfu-default) ; only needed with blend-background
              :config
              (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  


              (use-package consult-dir
               :bind (("M-g d" . consult-dir)
                      :map minibuffer-local-filename-completion-map
                      ("M-g d" . consult-dir)
                      ("M-s f" . consult-dir-jump-file)
                      ;; :map embark-become-file+buffer-map
                      ;; ("d" . consult-dir)
                      )
               :init
               (use-package vertico
                 :bind (:map vertico-map
                        ("M-g d" . consult-dir)
                        ("M-s f" . consult-dir-jump-file)
                        ))
                :config
               (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
               (setq consult-dir-shadow-filenames nil))

 
               (use-package yasnippet
                :demand t
                :diminish yas-minor-mode
                :commands yas-minor-mode-on
                :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
                :hook (prog-mode . yas-minor-mode-on)
                :custom
                (yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
                (yas-triggers-in-field t)
                (yas-wrap-around-region t)
                :custom-face
                (yas-field-highlight-face ((t (:background "#e4edfc"))))
                )

              (use-package consult-yasnippet
                :after (consult yasnippet))

                            ;; Minimising & quitting Emacs way too many times without wanting to.
                            (global-unset-key "\C-z")
                            (global-unset-key "\C-x\C-c")
                            (global-unset-key "\C-x\C-b") ;; list-buffer, i just use switch-buffer
                            (global-unset-key "\C-x\C-d") ;; list-directory, i just use dired

            '';

            usePackage = {
              exec-path-from-shell = {
                enable = true;
                extraConfig = ":when (daemonp)";
                config = "(exec-path-from-shell-initialize)";
              };

              gcmh = {
                enable = true;
                defer = 1;
                command = [ "gcmh-mode" ];
                config = ''
                  (setq gcmh-idle-delay 'auto)
                  (gcmh-mode)
                '';
              };
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
                  (setq auto-save-delete-trailing-whitespace nil)  ; automatically delete spaces at the end of the line when saving

                  ;;; custom predicates if you don't want auto save.
                  ;;; disable auto save mode when current filetype is an gpg file.
                  (setq auto-save-disable-predicates
                        '((lambda ()
                        (string-suffix-p
                        "gpg"
                        (file-name-extension (buffer-name)) t))))
                '';
              };

              ## Remember where we where in a previously visited file. Built-in.
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
                        history-length 100)
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

              keycast = {
                enable = true;
                command = [
                  "keycast-mode-line-mode"
                  "keycast-tab-bar-mode"
                  "keycast-header-line-mode"
                ];
              };

              free-keys = {
                enable = true;
                command = [ "free-keys" ];
              };

              orderless = {
                enable = true;
                config = ''
                  (setq completion-styles '(orderless basic))
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

              marginalia = {
                enable = true;
                config = "(marginalia-mode)";
              };

              embark = {
                enable = false;
                command = [ "embark-prefix-help-command" ];
                bind = {
                  "C-." = "embark-act";
                  "M-." = "embark-dwim";
                  "C-h B" = "embark-bindings";
                };
                init = ''
                  (setq prefix-help-command #'embark-prefix-help-command)
                '';
                config = ''
                  (setq embark-indicators '(embark-minimal-indicator
                                            embark-highlight-indicator
                                            embark-isearch-highlight-indicator))
                '';
              };


              corfu = {
                enable = false;
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
                  ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
              embark-consult = {
                enable = false;
                after = [ "embark" "consult" ];
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
                config = ''
                  (setq aw-keys '(?h ?a ?i ?o ?l ?u ?y ?')
                        aw-dispatch-always t)

                '';
              };

              winner = {
                enable = true;
                config = ''
                  (winner-mode 1)
                '';
              };

              hydra = {
                enable = true;
                config = ''
                              (defhydra hydra-window-menu (:color blue)
                              "window movement"
                               ("v" (lambda ()
                     (interactive)
                     (split-window-right)
                     (windmove-right)) "split right")
                  ("s" (lambda ()
                     (interactive)
                     (split-window-below)
                     (windmove-down)) "below")
                     ("k" delete-window "delete")
                           ("d" delete-other-windows "delete other")

                                                ("q" nil "cancel"))

                                (defhydra hydra-main-menu (:color blue)
                                                     "main menu"
                                                    ("p" project-switch-project "switch projects")
                                                    ("g" magit "magit")
                                                    ("n" org-roam-node-find "find note")
                                                    ("t" org-roam-dailies-goto-today "today note")
                                                    ("k" save-buffers-kill-emacs "quit emacs")
                                                    ("q" nil "cancel"))
                               (defhydra hydra-search-menu (:color teal)
                                   "search menu"
                                  ("l" consult-line "search line")
                                  ("r" consult-ripgrep "search word")
                                  ("f" consult-fd "searc file")
                                  ("q" nil "cancel"))

                                (global-set-key (kbd "C-c s") 'hydra-search-menu/body)
                                (global-set-key (kbd "C-c i") 'hydra-main-menu/body)
                                (global-set-key (kbd "C-c w") 'hydra-window-menu/body)
                '';
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
                hook = [
                  "(org-mode . org-modern-mode)"
                  "(org-agenda-finalize . org-modern-agenda)"
                ];
              };

              envrc = {
                enable = true;
                command = [ "envrc-mode" ];
              };


              org-roam = {
                enable = true;

                after = [ "org" ];
                init = ''
                  (setq org-roam-v2-ack t)
                '';
                config = ''
                          (use-package org-roam-dailies)

                   ;; If you're using a vertical completion framework, you might want a more informative completion interface
                  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
                  (setq org-roam-dailies-directory "daily/")
                    (setq org-roam-dailies-capture-templates
                       '(("d" "default" entry
                          "* %?"
                          :target (file+head "%<%Y-%m-%d>.org"
                                             "#+title: %<%Y-%m-%d>\n* Tasks to do \n* Journal \n* TIL \n"))))


                          (setq org-roam-directory (concat org-directory "roam/"))
                          (org-roam-db-autosync-mode)
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

                    (use-package denote-org-dblock)
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

              # Configure magit, a nice mode for the git SCM.
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
                '';
              };
              flycheck = {
                enable = true;
                config = ''
                  (global-flycheck-mode)
                  (flycheck-define-checker vale
                  "A checker for prose"
                  :command ("vale" "--output" "line"
                            source)
                  :standard-input nil
                  :error-patterns
                  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
                  :modes (markdown-mode gfm-mode org-mode text-mode))
                  (add-to-list 'flycheck-checkers 'vale 'append)
                '';
              };


              jinx = {
                enable = true;
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
                  "C-`" = "popper-toggle-latest";
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

              direnv.enable = true;
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
                  (markdown-command "pandoc -f markdown_github+smart")
                  (markdown-command-needs-filename t)
                  (markdown-enable-math t)
                  (markdown-open-command "marked")
                  :custom-face
                  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
                  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
                  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
                  (markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
                '';
                init = ''
                  (setq markdown-command "multimarkdown")
                '';
              };



              eglot = {
                enable = true;
                config = ''
                  (setq eglot-autoshutdown t)
                  (add-to-list 'eglot-server-programs
                              `(java-mode "jdtls-with-lombok"))
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
                  (pkgs.callPackage ./transient-showcase.nix {
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

              undo-tree = {
                enable = true;
                defer = 1;
                command = [
                  "global-undo-tree-mode"
                  "undo-tree-redo"
                  "undo-tree-undo"
                ];
                config = ''
                  (setq undo-tree-visualizer-relative-timestamps t
                        undo-tree-visualizer-timestamps t
                        undo-tree-enable-undo-in-region t
                        undo-tree-auto-save-history nil)
                  (global-undo-tree-mode)
                '';
              };

              lsp-bridge = {
                enable = true;
                package = epkgs: (
                  pkgs.callPackage ./lsp-bridge {
                    inherit (pkgs) fetchFromGitHub substituteAll writeText python3;
                    inherit lib;
                    inherit (epkgs) melpaBuild markdown-mode yasnippet;
                  }
                );
                hook = [ "(java-mode . lsp-bridge-mode)" ];
                config = ''
                  (require 'yasnippet)
                  (yas-global-mode 1)
                  (require 'lsp-bridge-jdtls)
                  (setq lsp-bridge-jdtls-jvm-args  (list (concat "-javaagent:" (getenv "LOMBOK_DIR") "/lombok.jar")))
                  (setq lsp-bridge-enable-auto-import t)
                '';
              };

              catppuccin-theme = {
                enable = true;
                defer = true;
                earlyInit = ''
                  ;; Set color theme in early init to avoid flashing during start.
                  (require 'catppuccin-theme)
                  (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, 'frappe or 'mocha
                  (load-theme 'catppuccin :no-confirm)
                '';
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
            };
          };

          home = {
            file.".emacs.d/snippets".source = ./config/snippets;
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
