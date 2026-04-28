;;; meow.el --- Meow configuration -*- lexical-binding: t; -*-
;;
;; Hot-reloadable Meow config — edit this file and reload with:
;;   M-x hm/reload-config
;; or enable auto-reload with:
;;   M-x hm-hot-reload-mode

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-define-key
   '("<escape>" . keyboard-escape-quit)
   )
  (meow-leader-define-key
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
   '("?" . meow-cheatsheet)
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
   '("<escape>" . ignore))
  ;; Avy: override meow-visit to use avy for jumping
  (define-key meow-normal-state-keymap (kbd "v") 'avy-goto-char-timer)
  (define-key meow-motion-state-keymap (kbd "v") 'avy-goto-char-timer)
  )

(meow-setup)
(meow-global-mode 1)
(setq meow-use-clipboard 't)
(add-to-list 'meow-mode-state-list '(nov-mode . insert))
(add-to-list 'meow-mode-state-list '(dired-mode . insert))

(defvar my-meow-off-modes
  '(vterm-mode term-mode shell-mode eshell-mode comint-mode dired-mode)
  "Major mode symbols: if the buffer `major-mode' derives from one of them,
`my-meow-off' turns off `meow-mode'.  Add modes with \\='(add-to-list \\='my-meow-off-modes \\='some-mode).")

(defun my-meow-off ()
  "Disable Meow in buffers where the major mode should use plain Emacs keys.
Runs from `after-change-major-mode-hook' with late priority so it wins over
`meow-global-mode', which re-enables `meow-mode' after major-mode hooks."
  (unless (minibufferp)
    (let ((modes my-meow-off-modes))
      (while modes
        (when (derived-mode-p (car modes))
          (meow-mode -1)
          (setq modes nil))
        (setq modes (cdr modes))))))

(add-hook 'after-change-major-mode-hook #'my-meow-off 100)

;;; Avy configuration
(setq avy-timeout-seconds 0.4)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m))

;;; Embark configuration
(setq embark-indicators
      '(embark-minibuffer-indicator embark-highlight-indicator))
(with-eval-after-load 'embark
  (setq which-key-enable-embark t)
  (with-eval-after-load 'consult
    (require 'embark-consult nil t)))

;;; meow.el ends here
