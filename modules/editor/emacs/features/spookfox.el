;;; spookfox.el --- Drive the active browser tab from Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Spookfox bridges Emacs and the browser over websockets: Emacs listens on
;; `spookfox-server--port', the signed add-on (packages/spookfox-firefox-extension.nix)
;; connects to it, and JS injected into the active tab comes back as Elisp data.
;;
;; Unlike upstream's docs, the server is *not* started at launch.  Nothing
;; listens until the first command runs or `my/spookfox-connect' is called, so
;; no local port is open for a session you never browse in.  After the server
;; (re)starts, click the Spookfox toolbar icon once to reconnect the add-on.
;;
;; Port of Sacha Chua's lisp/sacha-spookfox.el (sachac/.emacs.d), retargeted at
;; the Spookfox 0.8.1 API: `spookfox-js-injection-eval-in-active-tab' is now
;; `sfjsi-eval-in-active-tab', and `spookfox-init' / `spookfox-enabled-apps' are
;; gone.  Keys hang off the C-c w prefix map; see `my/spookfox-prefix-map'.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar my/spookfox--started nil
  "Non-nil once the Spookfox websockets server has been started here.")

(defun my/spookfox--load ()
  "Load Spookfox and its tabs and js-injection apps."
  (require 'spookfox)
  (require 'spookfox-tabs)
  (require 'spookfox-js-injection))

;;;###autoload
(defun my/spookfox-connect ()
  "Start the Spookfox websockets server the browser add-on connects to.
Click the add-on's toolbar icon afterwards if it does not reconnect by
itself; `my/spookfox-status' reports whether it got through."
  (interactive)
  (my/spookfox--load)
  (condition-case err
      (progn
        (spookfox-start-server)
        (setq my/spookfox--started t)
        (message "Spookfox listening on port %d -- click the add-on icon to connect"
                 spookfox-server--port))
    ;; Another Emacs (the daemon plus a one-off frame) already owns the port and
    ;; the add-on is talking to that one, so this instance cannot serve.
    (error
     (setq my/spookfox--started nil)
     (message "Spookfox: %s -- quit the other Emacs and run this again"
              (error-message-string err)))))

(defun my/spookfox--ensure-client ()
  "Make sure the server is up and a browser is connected, or bail out."
  (my/spookfox--load)
  (unless (and my/spookfox--started
               spookfox--server-process
               (process-live-p spookfox--server-process))
    (my/spookfox-connect))
  (when (seq-empty-p spookfox--connected-clients)
    (user-error "No browser connected -- click the Spookfox add-on icon")))

;;;###autoload
(defun my/spookfox-status ()
  "Report whether the server is up and how many browsers are connected."
  (interactive)
  (my/spookfox--load)
  (message "Spookfox: server %s, %d client(s) connected"
           (if (and spookfox--server-process (process-live-p spookfox--server-process))
               "up" "down")
           (length spookfox--connected-clients)))

(defun my/spookfox--eval-js (js)
  "Evaluate JS in the active tab and return the result as Elisp data.
Top-level arrays come back from `json-read' as vectors; flatten them to
lists so callers can pattern-match.  Nested arrays stay vectors -- use
`my/spookfox--->list' for those."
  (my/spookfox--ensure-client)
  (let ((result (sfjsi-eval-in-active-tab js t)))
    (if (vectorp result) (append result nil) result)))

(defun my/spookfox--->list (sequence)
  "Return SEQUENCE as a list of lists, turning decoded JSON vectors into lists."
  (mapcar (lambda (item) (if (vectorp item) (append item nil) item))
          (if (vectorp sequence) (append sequence nil) sequence)))

;;;; Capturing from the active tab

;;;###autoload
(defun my/spookfox-insert-url ()
  "Insert the active tab's URL at point."
  (interactive)
  (insert (my/spookfox--eval-js "window.location.href")))

;;;###autoload
(defun my/spookfox-insert-org-link ()
  "Insert an Org link to the active tab, titled with its page title."
  (interactive)
  (pcase-let ((`(,url ,title) (my/spookfox--eval-js "[window.location.href, document.title]")))
    (insert (org-link-make-string url title))))

;;;###autoload
(defun my/spookfox-link-to-fragment ()
  "Insert a Text Fragment link to the selection in the active tab."
  (interactive)
  (insert
   (my/spookfox--eval-js
    "window.location.href + '#:~:text=' + encodeURIComponent(window.getSelection().toString())")))

;;;; Picking links out of the page

(defun my/spookfox-get-links ()
  "Return the active tab's links as an alist of (URL . TITLE), deduplicated."
  (seq-uniq
   (mapcar (pcase-lambda (`(,url ,title)) (cons url (string-trim (or title ""))))
           (my/spookfox--->list
            (my/spookfox--eval-js
             "[...document.querySelectorAll('a[href]')].map(a => [a.href, (a.textContent || '').trim().replace(/\\s+/g, ' ')]).filter(([h]) => /^https?:/.test(h))")))))

(defun my/spookfox--read-link (&optional prompt)
  "Read a link from the active tab, completing on title and URL.
Return the chosen (URL . TITLE)."
  (let* ((links (my/spookfox-get-links))
         (candidates (mapcar (lambda (l) (format "%s  %s"
                                                 (if (string-empty-p (cdr l))
                                                     "(no title)" (cdr l))
                                                 (car l)))
                             links)))
    (when (seq-empty-p candidates)
      (user-error "No http(s) links in the active tab"))
    (let ((choice (completing-read (or prompt "Link: ") candidates nil t)))
      ;; Hrefs come from `a.href', so the browser has already percent-encoded any
      ;; space; the trailing URL is unambiguous.
      (unless (string-match " (https?://[^ ]+)\\'" choice)
        (user-error "No URL found in %S" choice))
      (let ((url (match-string 1 choice)))
        (cons url (or (cdr (assoc url links)) url))))))

;;;###autoload
(defun my/spookfox-insert-link-from-page (link)
  "Insert an Org link to LINK, picked from the active tab's links."
  (interactive (list (my/spookfox--read-link)))
  (insert (org-link-make-string (car link) (cdr link))))

;;;###autoload
(defun my/spookfox-open-link-from-page (link)
  "Open LINK, picked from the active tab's links, in a background tab."
  (interactive (list (my/spookfox--read-link "Open: ")))
  (my/spookfox--ensure-client)
  (if-let ((client (seq-first spookfox--connected-clients)))
      (spookfox-tabs--request client "OPEN_TAB" `(:url ,(car link)))
    (browse-url (car link))))

;;;; Reading without leaving Emacs

;;;###autoload
(defun my/spookfox-scroll-down ()
  "Page the active tab down, keeping focus in Emacs."
  (interactive)
  (my/spookfox--eval-js "window.scrollBy(0, document.documentElement.clientHeight)"))

;;;###autoload
(defun my/spookfox-scroll-up ()
  "Page the active tab up, keeping focus in Emacs."
  (interactive)
  (my/spookfox--eval-js "window.scrollBy(0, -document.documentElement.clientHeight)"))

;;;; Sending code to the page

;;;###autoload
(defun my/spookfox-send-region (start end)
  "Evaluate the region as JavaScript in the active tab."
  (interactive "r")
  (let ((result (my/spookfox--eval-js (buffer-substring-no-properties start end))))
    (message "=> %S" result)))

;;;###autoload
(defun my/spookfox-send-buffer ()
  "Evaluate the whole buffer as JavaScript in the active tab."
  (interactive)
  (my/spookfox-send-region (point-min) (point-max)))

;;;###autoload
(defun my/spookfox-send-line ()
  "Evaluate the current line as JavaScript in the active tab."
  (interactive)
  (my/spookfox-send-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun my/spookfox-send-expression ()
  "Evaluate from the line's indentation to point as JavaScript in the active tab."
  (interactive)
  ;; shortcut: sends the statement's line prefix, not a parsed expression;
  ;; upgrade to a js2-mode nav function if multi-line statements annoy you.
  (my/spookfox-send-region (save-excursion (back-to-indentation) (point)) (point)))

(defvar-keymap my/spookfox-js-minor-mode-map
  :doc "Send parts of the buffer to the active browser tab."
  "C-x C-e" #'my/spookfox-send-expression
  "C-c C-j" #'my/spookfox-send-line
  "C-c C-r" #'my/spookfox-send-region
  "C-c C-c" #'my/spookfox-send-buffer)

(define-minor-mode my/spookfox-js-minor-mode
  "Send JavaScript from this buffer to the active browser tab."
  :lighter " Spookfox"
  :keymap my/spookfox-js-minor-mode-map)

;;;; Org Babel

;;;###autoload
(defun my/org-babel-execute:js-spookfox (old-fn body params)
  "Run JS in the browser instead of a Node interpreter when :spookfox is set.
OLD-FN is the original `org-babel-execute:js', called with BODY and PARAMS."
  (if (assq :spookfox params)
      (my/spookfox--eval-js body)
    (funcall old-fn body params)))

(with-eval-after-load 'ob-js
  (advice-add #'org-babel-execute:js :around #'my/org-babel-execute:js-spookfox)
  ;; Org has no edit-prep method for js, so define the one ob-js would use.
  (cl-defmethod org-babel-edit-prep ((lang (eql js)) info)
    (when (assq :spookfox (nth 2 info))
      (my/spookfox-js-minor-mode 1))))

(with-eval-after-load 'embark-org
  (keymap-set embark-org-src-block-map "f" #'my/spookfox-eval-org-block))

;;;###autoload
(defun my/spookfox-eval-org-block ()
  "Evaluate the JavaScript src block at point in the active tab."
  (interactive)
  (require 'org)
  (let ((block (org-element-context)))
    (unless (and (eq (org-element-type block) 'src-block)
                 (member (org-element-property :language block) '("js" "javascript")))
      (user-error "Not in a js src block"))
    (my/spookfox-send-region
     (nth 2 (org-src--contents-area block))
     (nth 3 (org-src--contents-area block)))))

;;;; Keys

(defvar-keymap my/spookfox-prefix-map
  :doc "Browser-tab commands."
  "c" #'my/spookfox-connect
  "s" #'my/spookfox-status
  "u" #'my/spookfox-insert-url
  "l" #'my/spookfox-insert-org-link
  "f" #'my/spookfox-link-to-fragment
  "i" #'my/spookfox-insert-link-from-page
  "b" #'my/spookfox-open-link-from-page
  "n" #'my/spookfox-scroll-down
  "p" #'my/spookfox-scroll-up
  "e" #'my/spookfox-send-region)

(global-set-key (kbd "C-c w") my/spookfox-prefix-map)

(provide 'my-spookfox)
;;; spookfox.el ends here
