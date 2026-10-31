;;; org-refill.el --- Self-check for features/org-refill.nix -*- lexical-binding: t; -*-

;; Run from modules/editor/emacs:
;;   nix-instantiate --eval --strict --raw \
;;     -E 'let f = import ./features/org-refill.nix; in f.elisp' > /tmp/org-refill.el
;;   emacs -Q --batch -l /tmp/org-refill.el -l tests/org-refill.el
(require 'cl-lib)
(require 'org)
(require 'org-agenda)

(setq org-log-done nil org-log-refile nil org-startup-folded nil)

(let* ((tmp (make-temp-file "org-refill" t))
       (agenda-file (expand-file-name "projects.org" tmp))
       (window-file (expand-file-name "someday.org" tmp))
       (current-file (expand-file-name "inbox.org" tmp)))
  (with-temp-file agenda-file (insert "* Projects\n** Existing\n"))
  (with-temp-file window-file (insert "* Somewhere Else\n* Deep\n*** Too Deep\n"))
  (with-temp-file current-file (insert "* Local Heading\n* TODO moved item\n"))

  (let ((org-agenda-files (list agenda-file)))
    (with-current-buffer (find-file-noselect current-file)
      (org-mode)
      (display-buffer (find-file-noselect window-file))
      (goto-char (point-min))
      (re-search-forward "^\\* TODO")
      (let (captured-table)
        (cl-letf (((symbol-function 'org-refile-get-location)
                   (lambda (&optional _p _db _new)
                     (setq captured-table (org-refile-get-targets))
                     (list "Projects" agenda-file nil
                           (with-current-buffer (find-file-noselect agenda-file)
                             (save-excursion
                               (goto-char (point-min))
                               (re-search-forward "^\\* Projects")
                               (point)))))))
          (my/org-refill))
        (let ((names (mapcar #'car captured-table)))
          (message "targets: %S" names)
          (dolist (expected '("Projects" "Existing" "Somewhere Else"))
            (cl-assert (seq-some (lambda (n) (string-match-p (regexp-quote expected) n)) names)
              t "missing target %s in %S" expected names))
          (dolist (absent '("Local Heading" "Too Deep"))
            (cl-assert (not (seq-some (lambda (n) (string-match-p (regexp-quote absent) n)) names))
              t "unexpected target %s in %S" absent names))))
      (let ((content
             (with-current-buffer (find-file-noselect agenda-file)
               (buffer-substring-no-properties (point-min) (point-max)))))
        (cl-assert (string= content "* Projects\n** TODO moved item\n** Existing\n") t
                   "wrong layout: %S" content))
      (kill-buffer (get-file-buffer window-file)))))

(princ "org-refill: all assertions passed\n")
