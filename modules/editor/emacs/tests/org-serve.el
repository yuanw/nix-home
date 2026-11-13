;;; org-serve.el --- Self-check for features/org-serve.el -*- lexical-binding: t; -*-

;; Run against the running Emacs server from modules/editor/emacs:
;;   emacsclient --eval '(load (expand-file-name "tests/org-serve.el" default-directory) nil t)'
;; It only defines one function and calls it; no frames, no server ports.

(require 'cl-lib)

(load (expand-file-name "../features/org-serve.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun my/org-serve-self-check ()
  "Check `my/org-serve--copy-files'; return a one-line report."
  (let ((base (make-temp-file "org-serve-base" t))
        (root (make-temp-file "org-serve-root" t))
        (failures nil))
    (unwind-protect
        (let ((html (concat
                     "<img src=\"assets/pic.png\">"
                     "<video poster=\"clip.mp4\" src=\"clip.mp4\"></video>"
                     "<img src=\"two words.png\">"
                     "<img src=\"assets/pic%23x.png\">"
                     "<img src=\"assets/pic.png?v=2\">"
                     ;; Left alone: remote URL, anchor, missing file, rewritten link.
                     "<img src=\"https://example.com/remote.png\">"
                     "<a href=\"#top\">top</a>"
                     "<img src=\"nope.png\">"
                     "<img src=\"./pic.png\">")))
          (make-directory (expand-file-name "assets" base) t)
          (dolist (name '("assets/pic.png" "clip.mp4" "two words.png" "assets/pic#x.png"))
            (write-region "data" nil (expand-file-name name base)))
          (with-temp-buffer
            (insert html)
            (my/org-serve--copy-files root base)
            (dolist (name '("pic.png" "clip.mp4" "two words.png" "pic#x.png"))
              (unless (file-exists-p (expand-file-name name root))
                (push (format "not copied: %s" name) failures)))
            (dolist (name '("nope.png" "remote.png"))
              (when (file-exists-p (expand-file-name name root))
                (push (format "should not be copied: %s" name) failures)))
            ;; Each tag must point at the flat name; '#' goes back to %23.
            (dolist (tag '("<img src=\"./pic.png\">"
                           "poster=\"./clip.mp4\""
                           "src=\"./two words.png\""
                           "src=\"./pic%23x.png\""
                           "<img src=\"https://example.com/remote.png\">"
                           "<a href=\"#top\">"
                           "<img src=\"nope.png\">"
                           "<img src=\"./pic.png\">"))
              (goto-char (point-min))
              (unless (search-forward tag nil t)
                (push (format "missing in HTML: %s" tag) failures)))))
      (delete-directory base t)
      (delete-directory root t))
    (if failures
        (mapconcat #'identity (nreverse failures) "\n")
      "my/org-serve--copy-files: ok")))

(princ (my/org-serve-self-check))

;;; org-serve.el ends here
