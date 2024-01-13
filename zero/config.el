(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))



(eval-and-compile
  (defsubst emacs-path (path)
    (expand-file-name path user-emacs-directory))

  (setq package-enable-at-startup nil
        load-path
        (append (list (emacs-path "use-package"))
                (delete-dups load-path)
                (list (emacs-path "lisp")))))
(require 'use-package)

(setq use-package-verbose init-file-debug
      use-package-expand-minimally (not init-file-debug)
      use-package-compute-statistics t
      debug-on-error init-file-debug)

;; ;; Install a package via the elpaca macro
  ;; ;; See the "recipes" section of the manual for more details.

  ;; ;; (elpaca example-package)

  ;; ;; Install use-package support
  ;; (elpaca elpaca-use-package
  ;;   ;; Enable :elpaca use-package keyword.
  ;;   (elpaca-use-package-mode)
  ;;   ;; Assume :elpaca t unless otherwise specified.
  ;;   (setq elpaca-use-package-by-default t))

  ;; ;; Block until current queue processed.
  ;; (elpaca-wait)

  ;;When installing a package which modifies a form used at the top-level
  ;;(e.g. a package which adds a use-package key word),
  ;;use `elpaca-wait' to block until that package has been installed/configured.
  ;;For example:
  ;;(use-package general :demand t)
  ;;(elpaca-wait)
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
     '("?" . meow-cheatsheet))
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
  (use-package meow
    :demand t
    :config
    (meow-setup)
    (meow-global-mode 1)

    )

(use-package emacs
    :custom
    (tool-bar-mode nil)             ; Hide the outdated icons
    (scroll-bar-mode nil)           ; Hide the always-visible scrollbar
    (inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
    (use-file-dialog nil)      ; Ask for textual confirmation instead of GUI
    (menu-bar-mode nil)
    (tool-bar-mode nil)
    (scroll-bar-mode nil)
    :config (setq ring-bell-function #'ignore))

(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

(use-package emacs
  :init
	(when (eq system-type 'darwin)
		(setq mac-command-modifier 'super)
		(setq mac-option-modifier 'meta)
		(setq mac-control-modifier 'control)))

(use-package dired
   :commands dired-jump
  :diminish dired-omit-mode
   :hook
   (dired-mode . dired-hide-details-mode)
   (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'\\|^\\.DS_Store\\'\\|^\\.project\\(?:ile\\)?\\'\\|^\\.\\(?:svn\\|git\\)\\'\\|^\\.ccls-cache\\'\\|\\(?:\\.js\\)?\\.meta\\'\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")
  (dired-dwim-target t)
  :config

  )

(use-package keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  )

(use-package which-key
  :demand t
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit t
	  which-key-separator " → " )

    (which-key-mode)
  )

(set-face-attribute 'default nil
  :font "PragmataPro Mono Liga"
  :height 180
  :weight 'medium)

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-palenight t))

(use-package telephone-line
  :config

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24)
(setq telephone-line-evil-use-short-tag t)
(telephone-line-defsegment* telephone-line-simpler-major-mode-segment ()
  (concat "["
          (if (listp mode-name)
              (car mode-name)
            mode-name)
          "]"))

(telephone-line-defsegment* telephone-line-simple-pos-segment ()
  (concat "%c : " "%l/" (number-to-string (count-lines (point-min) (point-max)))))

(setq telephone-line-lhs
      '((nil . (telephone-line-projectile-buffer-segment))
        (accent . (telephone-line-simpler-major-mode-segment))
        (nil . (telephone-line-meow-tag-segment
                telephone-line-misc-info-segment)))
      telephone-line-rhs
      '((nil . (telephone-line-simple-pos-segment))
        (accent . (telephone-line-buffer-modified-segment))))

(telephone-line-mode 1)

)


(use-package nerd-icons)

(defun pixel-scroll-setup ()
  (interactive)
  (setq pixel-scroll-precision-large-scroll-height 1)
  (setq pixel-scroll-precision-interpolation-factor 1))

(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-setup)
  (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode)
  (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

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

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind  (([remap repeat-complex-command] . consult-complex-command)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap yank-pop] . consult-yank-pop)
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c K"   . consult-kmacro)
         ;; ("C-c i"   . consult-info)
         )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  ;; (consult-preview-key "M-i")
  (consult-narrow-key "<")

  :custom-face
  (consult-file ((t (:inherit font-lock-string-face))))

  :functions
  (consult-register-format
   consult-register-window
   consult-xref)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (use-package consult-xref)

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

  )

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; Enable vertico
(use-package vertico
  :config
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Minimising & quitting Emacs way too many times without wanting to.
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-c")

(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-dispatch-always t))

(use-package avy
  :bind ("C-." . avy-goto-char-timer)
  :custom
  (avy-case-fold-search t)
  (avy-timeout-seconds 0.3)
)

(require 'magit)
(use-package eglot
  :config
 (add-to-list 'eglot-server-programs
              `(java-mode "/opt/homebrew/bin/jdtls"
                           "-Djava.format.settings.url=https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
                           "-Djava.format.settings.profile=GoogleStyle"
                           ,(concat "--jvm-arg=-javaagent:" (expand-file-name "/Users/yuanwang/Downloads/lombok.jar"))))

  )

(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package nix-mode
  :mode "\\.nix\\'"
  :custom
  (nix-indent-function 'nix-indent-line))

(use-package haskell-mode
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-literate-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :bind (:map
         haskell-mode-map
         ("C-c C-h" . my-haskell-hoogle)
         ("C-c C-," . haskell-navigate-imports)
         ("C-c C-." . haskell-mode-format-imports)
         ("C-c C-u" . my-haskell-insert-undefined)
         ("C-c C-z" . haskell-interactive-switch)
         ("M-s")
         ("M-t"))
  :hook
  (haskell-mode . my-haskell-mode-hook)
  :custom
  (haskell-compile-cabal-build-command
   "cd %s && cabal new-build --ghc-option=-ferror-spans")
  (haskell-hasktags-arguments '("-e"))
  (haskell-tags-on-save t)
  (haskell-hoogle-command nil)
  (haskell-indent-spaces 2)
  (haskell-indentation-ifte-offset 2)
  (haskell-indentation-layout-offset 2)
  (haskell-indentation-left-offset 2)
  (haskell-indentation-starter-offset 2)
  (haskell-indentation-where-post-offset 2)
  (haskell-indentation-where-pre-offset 0)
  (haskell-process-args-cabal-repl
   '("--ghc-option=-ferror-spans"
     "--repl-options=-Wno-missing-home-modules"
     "--repl-options=-ferror-spans"))
  (haskell-process-load-or-reload-prompt t)
  :functions
  (haskell-check-remove-overlays
   haskell-goto-next-error
   haskell-goto-prev-error
   haskell-process-consume
   haskell-process-errors-warnings
   haskell-process-extract-modules
   haskell-process-import-modules
   haskell-process-reload-with-fbytecode
   haskell-process-response-cursor
   haskell-process-set-response-cursor
   haskell-session-name)
  :preface
  (defun my-haskell-insert-undefined ()
    (interactive) (insert "undefined"))

  (defun snippet (name)
    (interactive "sName: ")
    (find-file (expand-file-name (concat name ".hs") "~/src/notes"))
    (haskell-mode)
    (goto-char (point-min))
    (when (eobp)
      (insert "hdr")
      (yas-expand)))

  (defvar hoogle-server-process nil)

  (defun my-haskell-hoogle (query &optional _arg)
    "Do a Hoogle search for QUERY."
    (interactive
     (let ((def (haskell-ident-at-point)))
       (if (and def (symbolp def)) (setq def (symbol-name def)))
       (list (read-string (if def
                              (format "Hoogle query (default %s): " def)
                            "Hoogle query: ")
                          nil nil def)
             current-prefix-arg)))
    (let ((pe process-environment)
          (ep exec-path)
          ;; (default-hoo (expand-file-name
          ;;               "default.hoo"
          ;;               (locate-dominating-file "." "default.hoo")))
          )
      (unless (and hoogle-server-process
                   (process-live-p hoogle-server-process))
        (message "Starting local Hoogle server on port 8687...")
        (with-current-buffer (get-buffer-create " *hoogle-web*")
          (cd temporary-file-directory)
          (let ((process-environment pe)
                (exec-path ep))
            (setq hoogle-server-process
                  (start-process "hoogle-web" (current-buffer)
                                 (executable-find "hoogle")
                                 "server"
                                 ;; (concat "--database=" default-hoo)
                                 "--local" "--port=8687"))))
        (message "Starting local Hoogle server on port 8687...done")))
    (browse-url
     (format "http://127.0.0.1:8687/?hoogle=%s"
             (replace-regexp-in-string
              " " "+" (replace-regexp-in-string "\\+" "%2B" query)))))

  (defvar haskell-prettify-symbols-alist
    '(("::"     . ?∷)
      ("forall" . ?∀)
      ("exists" . ?∃)
      ("->"     . ?→)
      ("<-"     . ?←)
      ("=>"     . ?⇒)
      ("~>"     . ?⇝)
      ("<~"     . ?⇜)
      ("<>"     . ?⨂)
      ("msum"   . ?⨁)
      ("\\"     . ?λ)
      ("not"    . ?¬)
      ("&&"     . ?∧)
      ("||"     . ?∨)
      ("/="     . ?≠)
      ("<="     . ?≤)
      (">="     . ?≥)
      ("<<<"    . ?⋘)
      (">>>"    . ?⋙)

      ("`elem`"             . ?∈)
      ("`notElem`"          . ?∉)
      ("`member`"           . ?∈)
      ("`notMember`"        . ?∉)
      ("`union`"            . ?∪)
      ("`intersection`"     . ?∩)
      ("`isSubsetOf`"       . ?⊆)
      ("`isNotSubsetOf`"    . ?⊄)
      ("`isSubsequenceOf`"  . ?⊆)
      ("`isProperSubsetOf`" . ?⊂)
      ("undefined"          . ?⊥)))

  (defun my-update-cabal-repl (&rest _args)
    (let ((it (getenv "CABAL_REPL")))
      (when it
        (let ((args (nthcdr 2 (split-string it))))
          (setq-local haskell-process-args-cabal-repl
                      (delete-dups
                       (append haskell-process-args-cabal-repl args)))))))

  ;; (eval-when-compile
  ;;   (require 'diminish))

  (defun my-haskell-mode-hook ()
    (haskell-indentation-mode)
    (whitespace-mode 1)
    (bug-reference-prog-mode 1)

    (setq-local prettify-symbols-alist haskell-prettify-symbols-alist)
    (prettify-symbols-mode 1)

    (advice-add 'direnv-update-directory-environment
                :after #'my-update-cabal-repl)

    (when (executable-find "ormolu")
      (require 'format-all)
      (define-format-all-formatter
        ormolu
        (:executable "ormolu")
        (:install "stack install ormolu")
        (:languages "Haskell" "Literate Haskell")
        (:features)
        (:format
         (format-all--buffer-easy
          executable
          (when (buffer-file-name)
            (list "--stdin-input-file" (buffer-file-name))))))
      (format-all--set-chain "Haskell" '(ormolu))
      ;; (format-all-mode 1)
      ))
  :config
  (use-package align
    :defer t
    :config
    (add-to-list
     'align-rules-list
     (mapcar #'(lambda (x)
                 `(,(car x)
                   (regexp . ,(cdr x))
                   (modes quote (haskell-mode haskell-literate-mode))))
             '((haskell-types       . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (haskell-assignment  . "\\(\\s-+\\)=\\s-+")
               (haskell-arrows      . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (haskell-left-arrows . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+"))))))
