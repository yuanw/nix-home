;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "Yuan Wang"
;;       user-mail-address "yuan.wang@workiva.com")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq
 doom-theme 'doom-palenight
 ;; doom-theme 'doom-nano-dark-theme
 doom-font (font-spec :family "PragmataPro" :size 18))
;; (after! doom-themes
;;   (load-theme 'doom-nano-light t))
;; .doom.d/config.el
(setq  fancy-splash-image  "~/.config/wallpapers/doom.svg")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-roam-directory (concat org-directory "roam/")
      org-agenda-files (append
                        (file-expand-wildcards (concat org-directory "*.org"))
                        (file-expand-wildcards (concat org-directory "agenda/*.org"))
                        (file-expand-wildcards (concat org-directory "projects/*.org")))
      org-default-notes-file (concat org-directory "agenda/inbox.org")
      +org-capture-notes-file (concat org-directory "agenda/inbox.org")
      +org-capture-todo-file (concat org-directory "agenda/inbox.org")
       org-refile-targets '((+org/opened-buffer-files :maxlevel . 9))
      )
(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in Emacs."
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;
;;; Keybinds

(map!

 ;; (:after evil-org

 ;;       :map evil-org-mode-map
 ;;       :n "gk" (cmd! (if (org-on-heading-p)
 ;;                         (org-backward-element)
 ;;                       (evil-previous-visual-line)))
 ;;       :n "gj" (cmd! (if (org-on-heading-p)
 ;;                         (org-forward-element)
 ;;                       (evil-next-visual-line))))

 ;; :o "o" #'evil-inner-symbol

 :leader
 ;; (:prefix "n"
 ;;  "b" #'org-roam-buffer-toggle
 ;;  "d" #'org-roam-dailies-goto-today
 ;;  "D" #'org-roam-dailies-goto-date
 ;;  "i" #'org-roam-node-insert
 ;;  "r" #'org-roam-node-find
 ;;  "R" #'org-roam-capture)

 ;; (:prefix "i"
 ;;  "o" #'org-insert-todo-heading
 ;;  )
 (:prefix "t"
          "k" #'keycast-mode
          )

 )

(after! company
  (setq company-idle-delay 0.2))

;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
(advice-add #'doom-modeline-segment--modals :override #'ignore)

(setq org-agenda-custom-commands
      `(("d" "Dashboard"
         (
          ;; (agenda "" ((org-deadline-warning-days 3)))
          (todo "TODO"
                ((org-agenda-overriding-header "Work")
                 (org-agenda-files '("~/org/agenda/workiva.org"))
                 (org-agenda-text-search-extra-files nil)))

          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
          (tags-todo "+next"
                     ((org-agenda-overriding-header "Next Actions")
                      (org-agenda-max-todos nil)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                 (org-agenda-files '("~/org/agenda/inbox.org"))
                 (org-agenda-text-search-extra-files nil )))))

        ("i" "Next Tasks"
         (
          (tags-todo "+next"
                     ((org-agenda-overriding-header "Next Tasks")))))))

;; Disable invasive lsp-mode features
(setq
 ;; lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
 ;; lsp-ui-doc-enable nil        ; slow and redundant with K
 ;; lsp-enable-symbol-highlighting nil
 lsp-file-watch-threshold 5000
 ;; If an LSP server isn't present when I start a prog-mode buffer, you
 ;; don't need to tell me. I know. On some systems I don't care to have a
 ;; whole development environment for some ecosystems.
 ;;+lsp-prompt-to-install-server 'quiet
 )

(use-package! interaction-log)
(use-package! thrift-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.frugal\\'" . thrift-mode ) ))
(use-package! org-wild-notifier
  :config
  (after! org
    (org-wild-notifier-mode)
    (setq
     org-wild-notifier-alert-time '(60 30)
     alert-default-style 'osx-notifier))
  )
(after! browse-at-remote
  (setq browse-at-remote-add-line-number-if-no-region-selected t))

(add-transient-hook! 'focus-out-hook (atomic-chrome-start-server))
(after! org
  (setq org-eukleides-path (getenv "EUKLEIDES_PATH")
        org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup t
        org-agenda-use-tag-inheritance nil)
  (add-to-list 'org-modules 'org-habit)
  )
(use-package! super-save
  :config
  (add-to-list 'super-save-triggers 'vertico)
  (add-to-list 'super-save-triggers 'magit)
  (add-to-list 'super-save-triggers 'find-file)
  (add-to-list 'super-save-triggers 'winner-undo)
  (super-save-mode +1))

(use-package! embark-vc
  :after embark)
(use-package! evil-replace-with-register
  :config
  (setq evil-replace-with-register-key (kbd "gr"))
  (evil-replace-with-register-install))
;; (use-package! elgot-java
;;   :after elgot
;;   :config
;;   (
;;    (add-hook 'java-mode-hook 'eglot-java-mode)
;;    (add-hook 'eglot-java-mode-hook (lambda ()
;;                                      (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
;;                                      (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
;;                                      (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
;;                                      (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
;;                                      (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
;;                                      (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)))
;;    ))

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
      :height 0.9)
    '(keycast-key :inherit custom-modified
      :height 1.1
      :weight bold))
  )
;; (use-package! vlf-setup
;;   :defer-incrementally  vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)
(setq
 magit-list-refs-sortby "-committerdate"
 magit-log-section-commit-count 25
 ;; org-export-with-broken-links t
 ;; org-id-track-globally t
 ;; lsp-modeline-code-actions-enable nil
 ;;  lsp-modeline-diagnostics-enable nil

  )
(after! eglot
  :config
 (add-to-list 'eglot-server-programs
              `(java-mode "jdtls"
                           "-Djava.format.settings.url=https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
                           "-Djava.format.settings.profile=GoogleStyle"
                           ,(concat "--jvm-arg=-javaagent:" (expand-file-name "/Users/yuanwang/.m2/repository/org/projectlombok/lombok/1.18.20/lombok-1.18.20.jar"))))

  )


;; (setq lsp-java-configuration-runtimes '[(:name "JavaSE-17"
;; 						:path "/Users/yuanwang/.sdkman/candidates/java/17.0.5-amzn/"
;; 						:default t)])
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setf org-html-mathjax-template
      "<script src=\"https://polyfill.io/v3/polyfill.min.js?features=es6\"></script><script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>"
      )
