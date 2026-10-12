;;; agent-skill-org-agenda.el --- Pick and edit Org agenda TODOs  -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry points for coding agents, all callable over `emacsclient --eval':
;;
;;   (agent-skill-org-agenda-list &optional file keyword tag limit)
;;   (agent-skill-org-agenda-edit &optional file line heading subitems state note)
;;
;; Arguments are positional because this Emacs mishandles multi-key &key
;; arglists; positional/&optional calls work in every build.
;;
;; Mutations go through Org so buffers, LOGBOOK state changes and hooks stay
;; correct, including when the entry is already displayed in Emacs.

;;; Code:

(require 'org)
(require 'denote)

(defun agent-skill-org-agenda--clean (string)
  "Return STRING as one line, without text properties or spacing noise."
  (replace-regexp-in-string "[ \t\n]+" " " (org-no-properties string) t t))

(defun agent-skill-org-agenda--title ()
  "Return the current headline as a single-line title, without keyword or tags."
  (agent-skill-org-agenda--clean (org-get-heading t t t t)))

(defun agent-skill-org-agenda--keywords-used ()
  "Return every TODO keyword that can be set in the current buffer."
  (let (keywords)
    (dolist (sequence org-todo-keywords)
      (dolist (keyword sequence)
        (dolist (part (split-string (if (stringp keyword) keyword "") "|"))
          (when (string-match "^[[:alnum:]-]+" part)
            (push (match-string 0 part) keywords)))))
    (delete-dups keywords)))

(defun agent-skill-org-agenda--keywords-no-prompt ()
  "Return `org-todo-keywords' without `@'.
An `@' prompts for a note, which is impossible from `emacsclient --eval'.
Timestamp markers are kept so state changes are still logged."
  (mapcar (lambda (sequence)
            (mapcar (lambda (keyword)
                      (if (stringp keyword)
                          (replace-regexp-in-string "@" "" keyword)
                        keyword))
                    sequence))
          org-todo-keywords))

(defun agent-skill-org-agenda--agenda-files (&optional file)
  "Return the agenda files to search, most recently modified first.
With FILE search only that file; a bare name resolves below
`org-directory'/agenda/."
  (if (not file)
      (sort (seq-filter #'file-exists-p org-agenda-files)
            (lambda (a b)
              (< (float-time (nth 5 (file-attributes b)))
                 (float-time (nth 5 (file-attributes a))))))
    (let ((path (if (file-name-absolute-p file)
                    file
                  (expand-file-name file (expand-file-name "agenda/" org-directory)))))
      (unless (file-exists-p path)
        (user-error "No agenda file: %s" path))
      (list path))))

(defun agent-skill-org-agenda--entries (&optional file keyword tag)
  "Return open TODO entries in FILE as a list of (file line level state blocked tags title).
KEYWORD matches headings case-insensitively.  TAG filters by tag, ignoring case."
  (let ((entries nil)
        (case-fold-search t))
    (let ((org-agenda-files (agent-skill-org-agenda--agenda-files file)))
      (org-map-entries
       (lambda ()
         (let ((state (org-get-todo-state)))
           (when (and state (not (org-entry-is-done-p)))
             (let ((title (agent-skill-org-agenda--title))
                   (tags (mapcar #'downcase (delq nil (org-get-tags)))))
               (when (and (or (null keyword)
                              (string-match-p (regexp-quote keyword) title))
                          (or (null tag)
                              (member (downcase tag) tags)))
                 (push (list (buffer-file-name)
                             (line-number-at-pos (point))
                             (org-outline-level)
                             (agent-skill-org-agenda--clean state)
                             (and (org-entry-blocked-p) "BLOCKED")
                             tags
                             title)
                       entries))))))
       nil 'agenda))
    (nreverse entries)))

(defun agent-skill-org-agenda--describe-entry (entry)
  "Format ENTRY, one item of `agent-skill-org-agenda--entries'."
  (format "%s:%-4d %-7s %s%s%s"
          (file-name-nondirectory (nth 0 entry))
          (nth 1 entry)
          (nth 3 entry)
          (make-string (* 2 (max 0 (1- (nth 2 entry)))) ?\s)
          (if (nth 4 entry) "BLOCKED " "")
          (concat (nth 6 entry)
                  (mapconcat (lambda (entry-tag)
                               (concat " :" entry-tag ":"))
                             (nth 5 entry)
                             ""))))

;;;###autoload
(defun agent-skill-org-agenda-list (&optional file keyword tag limit)
  "List open TODO entries as a report string.
FILE restricts the search to one agenda file.  KEYWORD matches headings
case-insensitively.  TAG filters by tag, ignoring case.  LIMIT caps the report
(default 30); the total match count is always reported."
  (let* ((limit (or limit 30))
         (entries (agent-skill-org-agenda--entries file keyword tag))
         (shown (seq-take entries limit)))
    (concat
     (format "%d open item%s%s%s\n"
             (length entries)
             (if (= (length entries) 1) "" "s")
             (if keyword (format " matching %S" keyword) "")
             (if tag (format " tagged :%s:" tag) ""))
     (mapconcat #'agent-skill-org-agenda--describe-entry shown "\n")
     (if (> (length entries) (length shown))
         (format "\n... %d more.  Narrow with KEYWORD, TAG or FILE."
                 (- (length entries) (length shown)))
       ""))))

(defun agent-skill-org-agenda--note-file (note)
  "Resolve Denote NOTE, an identifier or a path, to a note file."
  (if (file-name-absolute-p note)
      (expand-file-name note)
    ;; A plain directory scan: Denote's own lookup helpers are unavailable in
    ;; some versions, and `denote-directory-files' is expensive on large trees.
    (or (seq-find (lambda (name)
                    (string-match-p (regexp-quote note) (file-name-nondirectory name)))
                  (ignore-errors
                    (directory-files (expand-file-name denote-directory) t "\\.org\\'")))
        (user-error "No Denote note matches %S; pass the note file path instead" note))))

(defun agent-skill-org-agenda--note-title (file)
  "Return the title part of Denote note FILE's name, e.g. \"api-key-rotation\"."
  (let ((base (file-name-sans-extension (file-name-nondirectory file))))
    (if (string-match "--\\(.+\\)$" base)
        (replace-regexp-in-string "__.*" "" (match-string 1 base))
      base)))

(defun agent-skill-org-agenda--link-note (note)
  "Link the current entry to Denote NOTE, an identifier or a path.
Return a description of the link."
  (let* ((file (agent-skill-org-agenda--note-file note))
         (base (file-name-nondirectory file))
         (id (and (string-match "[0-9]\\{8\\}T[0-9]\\{6\\}" base)
                  (match-string 0 base)))
         (title (agent-skill-org-agenda--note-title file)))
    (unless (and id (file-exists-p file))
      (user-error "Cannot link %S: not a Denote note" note))
    ;; Just below the heading, after the drawer if there is one.  Never further:
    ;; `org-end-of-meta-data' walks into the next sibling when the body is empty.
    (org-back-to-heading)
    (goto-char (1+ (line-end-position)))
    (when (and (looking-at "[ 	]*:[ 	]*[A-Za-z][A-Za-z-]*:")
               (re-search-forward "^[ 	]*:END:[ 	]*$" nil t))
      (forward-line 1))
    (insert (format "- [[denote:%s][%s]]\n" id title))
    (format "linked \"%s\"" title)))

(defun agent-skill-org-agenda--goto (file line heading)
  "Return the buffer for FILE positioned on LINE, verified against HEADING.
Line numbers move as entries are added or done, so refuse to edit an entry
whose heading no longer matches what the agent was shown."
  (let ((buffer (find-file-noselect (car (agent-skill-org-agenda--agenda-files file)))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (unless (zerop (forward-line (1- line)))
        (user-error "%s has fewer than %d lines" (buffer-name) line))
      (unless (org-at-heading-p)
        (user-error "Line %d of %s is not a heading" line (buffer-name)))
      (when heading
        (let ((found (downcase (replace-regexp-in-string "[^[:alnum:]]" ""
                                                         (agent-skill-org-agenda--title))))
              (want (downcase (replace-regexp-in-string "[^[:alnum:]]" ""
                                                        heading))))
          (unless (or (string-empty-p want)
                      (string-empty-p found)
                      (string-search want found)
                      (string-search found want))
            (user-error "Line %d is now \"%s\", not \"%s\"; list again"
                        line
                        (agent-skill-org-agenda--title)
                        heading))))
      buffer)))

(defun agent-skill-org-agenda--insert-subitems (titles)
  "Insert a TODO child heading for each string in TITLES, return the count."
  (let ((level (org-outline-level))
        (end (save-excursion (org-end-of-subtree t t))))
    (goto-char end)
    (unless (bolp)
      (insert "\n"))
    (dolist (title titles)
      (insert (make-string (1+ level) ?*)
              " TODO "
              (agent-skill-org-agenda--clean title)
              "\n"))
    (unless (bolp)
      (insert "\n"))
    (length titles)))

(defun agent-skill-org-agenda--set-state (state)
  "Set the current entry TODO keyword to STATE, return the change text."
  (unless (member state (agent-skill-org-agenda--keywords-used))
    (user-error "Unknown state %S (known: %s)"
                state
                (mapconcat #'identity (agent-skill-org-agenda--keywords-used) " ")))
  (let ((before (agent-skill-org-agenda--clean (or (org-get-todo-state) "none")))
        ;; Drop the note prompts, which cannot be answered over emacsclient.
        (org-todo-keywords (agent-skill-org-agenda--keywords-no-prompt)))
    (org-todo state)
    (format "state \"%s\" -> \"%s\""
            before
            (agent-skill-org-agenda--clean (or (org-get-todo-state) "none")))))

;;;###autoload
(defun agent-skill-org-agenda-edit (&optional file line heading subitems state note)
  "Edit one agenda entry.  FILE and LINE locate it, HEADING must still match.
SUBITEMS is a list of new child TODO headings.  STATE is a TODO keyword to
change to.  NOTE is a Denote identifier or path linked into the entry."
  (unless line
    (user-error "LINE is required, run agent-skill-org-agenda-list first"))
  (let ((changes '())
        (buffer (agent-skill-org-agenda--goto file
                                             line
                                             (and heading
                                                  (agent-skill-org-agenda--clean heading))))
        entry
        here)
    (unwind-protect
        (with-current-buffer buffer
          ;; Inserting entries moves point, so remember the entry and re-anchor
          ;; before every step.
          (setq entry (agent-skill-org-agenda--title)
                here (point-marker))
          (when subitems
            (goto-char here)
            (let ((parent (agent-skill-org-agenda--title))
                  (count (agent-skill-org-agenda--insert-subitems
                          (if (listp subitems) subitems (list subitems)))))
              (push (format "added %d sub-item%s under \"%s\""
                            count
                            (if (= count 1) "" "s")
                            parent)
                    changes)))
          (when state
            (goto-char here)
            (push (agent-skill-org-agenda--set-state state) changes))
          (when note
            (goto-char here)
            (push (agent-skill-org-agenda--link-note note) changes))
          (save-buffer))
      (when (buffer-modified-p buffer)
        (with-current-buffer buffer
          (save-buffer))))
    (format "%s:%d  %s  |  %s"
            (file-name-nondirectory (buffer-file-name buffer))
            line
            entry
            (if changes
                (mapconcat #'identity (nreverse changes) "; ")
              "no change"))))

(provide 'agent-skill-org-agenda)
;;; agent-skill-org-agenda.el ends here
