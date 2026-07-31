;;; hel-consult.el --- Hel + Consult integration -*- lexical-binding: t; -*-
;;
;; Hel-specific Consult keybindings and options, adapted from helheim-consult.
;; Loaded when modalEditing = "hel" in nix-home.

(require 'hel-leader)

(with-eval-after-load 'consult
  (setq consult-line-start-from-top t
        consult-narrow-key "<"
        consult-fd-args `(,(if (executable-find "fdfind" 'remote) "fdfind" "fd")
                          "--color=never"
                          "--full-path --absolute-path"
                          "--hidden --exclude .git")
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-async-refresh-delay 0.15))

(with-eval-after-load 'consult
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult-source-bookmark
   consult-source-file-register
   consult-source-recent-file
   consult-source-project-recent-file
   :preview-key '(:debounce 0.2 any))
  (consult-customize consult-theme
                     :preview-key '(:debounce 0.5 any)))

;; C-/ is line search; "/" on the leader is project ripgrep.
(hel-keymap-global-set :state '(normal emacs)
  "C-/" 'consult-line
  "C-?" 'consult-line-multi)

(hel-keymap-global-set :state 'normal
  "g o" 'consult-imenu
  "g O" 'consult-imenu-multi
  "g p" 'consult-outline
  "g m" 'consult-mark
  "g M" 'consult-global-mark
  "g e" 'consult-compile-error
  "g <return>" 'consult-goto-line)

(hel-keymap-set minibuffer-local-map
  "C-r" 'consult-history)

(hel-keymap-set search-map
  "f" 'consult-fd
  "l" 'consult-locate
  "g" 'consult-grep
  "v" 'consult-git-grep
  "/" 'consult-ripgrep
  "i" 'consult-info
  "k" 'consult-keep-lines
  "u" 'consult-focus-lines)

(hel-keymap-set project-prefix-map
  "b" 'consult-project-buffer)

(hel-keymap-set mode-specific-map
  "/" 'consult-ripgrep
  "f /" 'consult-fd)

(with-eval-after-load 'embark-consult
  (require 'embark-consult nil t))

;;; hel-consult.el ends here
