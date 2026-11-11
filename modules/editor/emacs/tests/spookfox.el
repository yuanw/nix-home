;;; spookfox.el --- Self-check for features/spookfox.el -*- lexical-binding: t; -*-

;; Run against the running Emacs server from modules/editor/emacs:
;;   emacsclient --eval '(load (expand-file-name "tests/spookfox.el" default-directory) nil t)'
;; Nothing here talks to the browser: the JS round trip is stubbed, so no
;; websockets server is started and no frames are touched.

(require 'cl-lib)

(load (expand-file-name "../features/spookfox.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun my/spookfox-self-check ()
  "Check the pure-Elisp parts of the Spookfox helpers; return a report."
  (let ((failures nil))
    ;; A js block with :spookfox goes to the browser, one without it to ob-js.
    (let (browser-js old-called)
      (cl-letf (((symbol-function 'my/spookfox--eval-js)
                 (lambda (js) (setq browser-js js) "from-browser")))
        (unless (equal "from-browser"
                       (my/org-babel-execute:js-spookfox
                        (lambda (&rest _) (setq old-called t) "from-node")
                        "1 + 1" '((:spookfox . t) (:exports . "results"))))
          (push "advice did not return the browser result" failures))
        (unless (equal browser-js "1 + 1")
          (push (format "browser got %S, expected the block body" browser-js) failures))
        (when old-called
          (push "advice called ob-js for a :spookfox block" failures))
        (setq browser-js nil)
        (unless (equal "from-node"
                       (my/org-babel-execute:js-spookfox
                        (lambda (_body _params) "from-node")
                        "1 + 1" '((:exports . "results"))))
          (push "advice did not fall through to ob-js" failures))))
    ;; json-read hands back vectors; the link list must become an alist anyway.
    (let ((raw (vector (vector "https://a.example" "  First ")
                       (vector "https://a.example" "  First ")
                       (vector "https://b.example" ""))))
      (cl-letf (((symbol-function 'my/spookfox--eval-js) (lambda (_js) raw)))
        (let ((links (my/spookfox-get-links)))
          (unless (equal links '(("https://a.example" . "First")
                                 ("https://b.example" . "")))
            (push (format "get-links gave %S" links) failures)))))
    (let ((mixed (vector (cons "a" "b") (vector "c" "d"))))
      (unless (equal (my/spookfox--->list mixed) '(("a" . "b") ("c" "d")))
        (push (format "-->list gave %S" (my/spookfox--->list mixed)) failures)))
    (if failures
        (mapconcat #'identity (nreverse failures) "\n")
      "my/spookfox: ok")))

(princ (my/spookfox-self-check))

;;; spookfox.el ends here
