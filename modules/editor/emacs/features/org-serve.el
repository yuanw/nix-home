;;; org-serve.el --- Export Org to HTML and serve it so local media plays -*- lexical-binding: t; -*-

;;; Commentary:

;; "Export to HTML file and open" hands the browser a file:// URL, so <video>
;; and <audio> tags pointing at local files stay dead.  This exports into a
;; temporary directory instead, flattens every referenced local file into it,
;; rewrites the tags to point at the flat names, and serves it with
;; simple-httpd on loopback.
;;
;; Run `my/org-serve' (or `my/org-serve-subtree'), or use C-c C-e s b / s s.
;; Port of Sacha Chua's sacha-org-export-and-serve; his media-file copying is
;; kept, the 11ty and permalink plumbing is not.

;;; Code:

(require 'cl-lib)

(defcustom my/org-serve-port 8085
  "Port the local server started by `my/org-serve' listens on."
  :type 'natnum
  :group 'org-export-html)

(defvar my/org-serve--root nil
  "Temporary directory created by `my/org-serve--root', or nil.")

(defun my/org-serve--root ()
  "Return the directory to serve, creating it once per session."
  (or my/org-serve--root
      (setq my/org-serve--root (make-temp-file "org-serve" t))))

(defun my/org-serve--cleanup ()
  "Delete the served directory, but only if we created it under /tmp."
  (when (and my/org-serve--root
             (file-in-directory-p my/org-serve--root temporary-file-directory))
    (delete-directory my/org-serve--root t)
    (setq my/org-serve--root nil)))

(defun my/org-serve--copy-files (root base-dir)
  "Copy local files referenced in the current HTML buffer into ROOT.
Relative links are resolved against BASE-DIR, and each tag is rewritten to
the flat name now living in ROOT.  Remote URLs, anchors and data URIs are
left alone.  Scanning only moves forward, so a rewritten tag is not
processed a second time."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(?:src\\|href\\|poster\\)=\"\\([^\"]*\\)\"" nil t)
      (let* ((raw (match-string 1))
             (path (replace-regexp-in-string
                    "%23" "#"
                    (string-remove-prefix
                     "file://" (replace-regexp-in-string "[?#].*\\'" "" raw)))))
        (when (and (not (string-empty-p path))
                   (not (string-prefix-p "#" path))
                   (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" path))
                   (file-exists-p (expand-file-name path base-dir)))
          (let ((source (expand-file-name path base-dir))
                (target (expand-file-name (file-name-nondirectory path) root)))
            (when (or (not (file-exists-p target))
                      (file-newer-than-file-p source target))
              (copy-file source target t))
            (when (file-exists-p target)
              (replace-match
               (concat "./" (replace-regexp-in-string "#" "%23"
                                                      (file-name-nondirectory path)))
               t t nil 1))))))))

(defun my/org-serve--ensure-backend ()
  "Define the `my-html-served' backend once ox-html is available."
  (require 'ox-html)
  (unless (org-export-get-backend 'my-html-served)
    (org-export-define-derived-backend 'my-html-served 'html
      :menu-entry
      '(?s "Export to HTML and serve"
           ((?b "As buffer" my/org-serve)
            (?s "As subtree" my/org-serve-subtree))))))

(with-eval-after-load 'ox-html
  (my/org-serve--ensure-backend))

(defun my/org-serve--export (subtreep)
  "Export this Org buffer, or the subtree if SUBTREEP, and serve the HTML."
  (my/org-serve--ensure-backend)
  (require 'simple-httpd)
  (unless buffer-file-name
    (user-error "Save the buffer before serving it"))
  (let* ((root (my/org-serve--root))
         (base-dir (file-name-directory buffer-file-name))
         (out-file (expand-file-name (concat (file-name-base buffer-file-name) ".html")
                                     root)))
    (org-export-to-file 'my-html-served out-file nil subtreep)
    (with-temp-file out-file
      (insert-file-contents out-file)
      (my/org-serve--copy-files root base-dir))
    ;; `httpd-host' nil would bind every interface; 'local keeps it on loopback.
    (setq httpd-host 'local
          httpd-port my/org-serve-port
          httpd-root root)
    (httpd-start)
    (browse-url (format "http://127.0.0.1:%d/%s"
                        httpd-port (file-name-nondirectory out-file)))
    (message "Serving %s on http://127.0.0.1:%d -- M-x httpd-stop to stop"
             root httpd-port)))

;;;###autoload
(defun my/org-serve (&optional _async)
  "Export the whole buffer to HTML and serve it from a temporary directory."
  (interactive)
  (my/org-serve--export nil))

;;;###autoload
(defun my/org-serve-subtree (&optional _async)
  "Export the subtree at point to HTML and serve it from a temporary directory."
  (interactive)
  (my/org-serve--export t))

(add-hook 'kill-emacs-hook #'my/org-serve--cleanup)

(provide 'my-org-serve)
;;; org-serve.el ends here
