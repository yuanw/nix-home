;;; rex-gt.el --- Remote control for the Rex terminal multiplexer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: yuan
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, processes, terminals
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; rex-gt drives a running Rex (https://www.superlogical.com/rex/) from
;; Emacs through the `rex' command-line tool, the way kitty-gt drives
;; Kitty over its remote-control socket.
;;
;; One call gets the whole picture.  `rex-gt-gather' runs a small Lua
;; script with `rex do -e', which attaches to every session once and
;; asks the server for each window, each pane, each pane's foreground
;; process, its OSC title, and the status programs report with OSC 7501
;; (working / blocked / done / idle / error).  That single snapshot
;; feeds every command here, so a refresh costs one process instead of
;; one `rex' call per pane.
;;
;; Commands:
;;
;;   M-x rex-gt-tab-manager   `*rex-gt-panes*': one row per pane across
;;                            every session.  RET shows it in the Rex
;;                            app, c captures its screen, d / D open
;;                            Dired on its cwd, r renames the tab, R
;;                            the pane, s cycles the sort key, g
;;                            refreshes.  A `rex events' process pushed
;;                            over by the server drives the updates
;;                            while the table is on screen, with a slow
;;                            poll behind it.
;;   M-x rex-gt-go            Jump to a pane by name.
;;   M-x rex-gt-capture       Read one pane's screen and scrollback.
;;   M-x rex-gt-new-tab       Add a tab to a session.
;;   M-x rex-gt-focus-app     Raise the Rex application.
;;
;; The other direction works too: `rex-gt-open-pane-directory' is what
;; Rex's ctrl+n>e key calls, through emacsclient, so a pane can open its
;; own directory in Emacs.  That binding lives in
;; modules/terminal-multiplexer/rex.nix.
;;
;; App-side actions (showing a session, raising a window) need Rex's
;; Remote Control setting on: `rex.client.remoteControl' true.  The
;; server-side half of every command works without it, so with Remote
;; Control off the table still reads correctly and only the jump stays
;; in Emacs.
;;
;; If `rex' is not on the PATH Emacs sees, set `rex-gt-program' to its
;; absolute path.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup rex-gt nil
  "Remote control for the Rex terminal multiplexer."
  :group 'processes
  :prefix "rex-gt-")

(defcustom rex-gt-program "rex"
  "Program used for every Rex call.
Give an absolute path when `rex' is not on the PATH Emacs inherits."
  :type 'string
  :group 'rex-gt)

(defcustom rex-gt-client nil
  "Label or ID of the Rex app to drive, or nil to let Rex choose.
Rex picks for itself when exactly one app is connected; set this when
two apps (a laptop and a Mac mini, say) are connected at once."
  :type '(choice (const :tag "Let Rex choose" nil) string)
  :group 'rex-gt)

(defcustom rex-gt-app-name "Rex"
  "macOS application to raise after focusing a pane, nil to leave alone.
Only consulted where `osascript' exists."
  :type '(choice (const :tag "Do not raise an app" nil) string)
  :group 'rex-gt)

(defcustom rex-gt-refresh-interval 60
  "Seconds between fallback polls in `rex-gt-tab-manager'.
Changes normally arrive through the `rex events' stream, which updates
the table as they happen; this poll only covers a stream that died
mid-session.  nil leaves recovery to `g'."
  :type '(choice (const :tag "No polling" nil)
                 (number :tag "Seconds"))
  :group 'rex-gt)

(defconst rex-gt--gather-script
  "local out = {}
rex.call('session.attach_all', {event_scope = 'all'})

local function newest(records)
  local best
  for _, r in pairs(records or {}) do
    if type(r) == 'table'
        and (not best or tostring(r.updated_at or '') > tostring(best.updated_at or '')) then
      best = r
    end
  end
  return best
end

local function call(session_id, block_id, method)
  local ok, result = pcall(rex.call, 'block.method',
      {session_id = session_id, block_id = block_id, method = method})
  if ok then return result end
end

for _, s in ipairs((rex.call('session.list', {}) or {}).sessions or {}) do
  local sid = s.session_id
  local ok, view = pcall(rex.call, 'session.view', {session_id = sid})
  if ok and view then
    for _, w in ipairs(view.windows or {}) do
      for _, l in ipairs(w.layers or {}) do
        for _, b in ipairs(l.blocks or {}) do
          local proc = call(sid, b.block_id, 'process') or {}
          local fore = proc.foreground or {}
          local titled = call(sid, b.block_id, 'title') or {}
          local status = newest((call(sid, b.block_id, 'program_status') or {}).records) or {}
          out[#out + 1] = {
            session_id = sid,
            session_label = s.label,
            window_id = w.window_id,
            window_label = w.label,
            active_window = view.active_window_id == w.window_id,
            focused = w.focused_block_id == b.block_id,
            block_id = b.block_id,
            block_label = b.label,
            flavor = b.flavor,
            layer_kind = l.kind,
            title = titled.title,
            cmd = fore.argv0 or fore.name,
            pid = fore.pid,
            cwd = fore.cwd,
            state = status.state,
            state_kind = status.kind,
            state_msg = status.msg,
            app = status.effective_app or status.app,
            state_at = status.updated_at,
          }
        end
      end
    end
  end
end

print(rex.json.encode(out))
"
  "Lua run with `rex do -e' to snapshot every session, window and pane.

Runs on the server side, so one process answers the whole table:
`session.attach_all' is what lets a single connection ask about
sessions other than its own.")

;; ============================================================================
;; CLI bridge
;; ============================================================================

(defun rex-gt--run (args &optional noerror)
  "Run the Rex CLI with ARGS and return its stdout as a string.
Signal a `user-error' when it fails, or return nil when NOERROR is non-nil."
  (unless (executable-find rex-gt-program)
    (if noerror
        nil
      (user-error "Rex CLI `%s' not on PATH; set `rex-gt-program'" rex-gt-program)))
  (let ((buffer (generate-new-buffer " *rex-gt*"))
        ;; Rex finds its server through the user environment, and
        ;; `default-directory' may name a worktree that is gone.
        (default-directory (expand-file-name "~"))
        status output)
    (unwind-protect
        (progn
          (setq status
                (apply #'call-process rex-gt-program nil (list buffer nil) nil args))
          (setq output (string-trim (with-current-buffer buffer (buffer-string)))))
      (kill-buffer buffer))
    (cond
     ((zerop status) output)
     (noerror nil)
     (t
      (user-error "rex %s failed (%d): %s"
                  (string-join args " ") status
                  (if (string-empty-p output) "no output" output))))))

(defun rex-gt--read-json (output)
  "Parse OUTPUT, JSON from the Rex CLI, into alists and lists."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol)
        (json-false nil)
        (json-null nil))
    (json-read-from-string (string-trim output))))

(defun rex-gt--json (args)
  "Run the Rex CLI with ARGS and return its JSON reply, parsed."
  (rex-gt--read-json (rex-gt--run args)))

(defun rex-gt-gather ()
  "Return every pane in every Rex session as a list of alists.
Each pane carries session, window and block IDs and labels, which pane
is focused, its OSC title, foreground pid, command, cwd, and the status
the program in it reported.  Signals a `user-error' when Rex cannot be
reached."
  (append (rex-gt--json (list "do" "-e" rex-gt--gather-script)) nil))

(defun rex-gt-gather-async (callback)
  "Fetch `rex-gt-gather' without blocking, then call CALLBACK with the panes.
On failure report in the echo area and skip CALLBACK.  Returns the
process, so callers can tell whether a fetch is already running."
  (let ((buffer (generate-new-buffer " *rex-gt-async*"))
        (default-directory (expand-file-name "~")))
    (make-process
     :name "rex-gt-gather"
     :buffer buffer
     :command (list (or (executable-find rex-gt-program) rex-gt-program)
                    "do" "-e" rex-gt--gather-script)
     :noquery t
     :sentinel
     (lambda (process _event)
       (unless (process-live-p process)
         (unwind-protect
             (if (zerop (process-exit-status process))
                 (condition-case err
                     (funcall callback
                              (append (rex-gt--read-json (with-current-buffer buffer (buffer-string)))
                                      nil))
                   (json-error
                    (message "rex-gt: unreadable reply from `rex do': %s"
                             (error-message-string err))))
               (message "rex-gt: refresh failed: %s"
                        (string-trim (with-current-buffer buffer (buffer-string)))))
           (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun rex-gt-running-p ()
  "Return non-nil when a Rex server is reachable.
Never starts a server of its own, unlike a plain `rex' call."
  (condition-case nil
      (and (rex-gt--run (list "--autostart=false" "ls" "--json") t) t)
    (error nil)))

(defun rex-gt--client-action (action &rest args)
  "Ask a Rex app to perform client ACTION with ARGS.
ARGS is a list of (KEY . VALUE) strings.  Honors `rex-gt-client'."
  (rex-gt--run (append (when rex-gt-client (list "-C" rex-gt-client))
                       (list "do" action)
                       (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair)))
                               args))))

(defun rex-gt--sessions ()
  "Return the sessions as a list of (DISPLAY . SESSION-ID)."
  (mapcar (lambda (session)
            (let ((id (alist-get 'session_id session)))
              (cons (format "%s (%s)"
                            (or (alist-get 'label session) "(unlabeled)")
                            (string-remove-prefix "session:" id))
                    id)))
          (alist-get 'sessions (rex-gt--json (list "ls" "--json")))))

;; ============================================================================
;; Panes: naming, focusing, reading
;; ============================================================================

(defun rex-gt--pane-description (pane)
  "Return a completion/label line for PANE."
  (format "%s / %s / %s  [%s%s]"
          (or (alist-get 'session_label pane) "?")
          (or (alist-get 'window_label pane) "?")
          (or (alist-get 'title pane) (alist-get 'block_label pane) "?")
          (let ((cmd (alist-get 'cmd pane)))
            (or (and cmd (file-name-nondirectory cmd)) "?"))
          (if-let* ((pid (alist-get 'pid pane))) (format " %d" pid) "")))

(defun rex-gt-read-pane (prompt &optional panes)
  "Read a pane by name with PROMPT and return its data.
PANES defaults to `rex-gt-gather'."
  (let* ((panes (or panes (rex-gt-gather)))
         (table (mapcar (lambda (pane) (cons (rex-gt--pane-description pane) pane)) panes))
         (choice (completing-read prompt (mapcar #'car table) nil t)))
    (or (cdr (assoc choice table))
        (user-error "No such Rex pane"))))

(defun rex-gt-focus-app ()
  "Raise the Rex application."
  (interactive)
  (when (and rex-gt-app-name (executable-find "osascript"))
    (call-process "osascript" nil nil nil "-e"
                  (format "tell application %S to activate" rex-gt-app-name))))

(defun rex-gt-focus-pane (pane)
  "Show PANE in the Rex app and give it the focus.
Sets the server-side focus first, so the pane is selected even with
Remote Control off, then asks the app to display its session."
  (let ((session (alist-get 'session_id pane))
        (window (alist-get 'window_id pane))
        (block (alist-get 'block_id pane)))
    (rex-gt--run (list "focus" "-s" session block))
    (condition-case err
        (rex-gt--client-action "session.select"
                               (cons "session_id" session)
                               (cons "window_id" window))
      (error
       (message "rex-gt: the app did not follow (%s)" (error-message-string err))))
    (rex-gt-focus-app)))

;;;###autoload
(defun rex-gt-go ()
  "Jump to a Rex pane: show its session and tab in the app and focus it."
  (interactive)
  (rex-gt-focus-pane (rex-gt-read-pane "Rex pane: ")))

;;;###autoload
(defun rex-gt-new-tab (session label directory)
  "Create a Rex tab named LABEL running a shell in DIRECTORY in SESSION.
Interactively, SESSION is chosen from the running ones and DIRECTORY
defaults to the current buffer's."
  (interactive
   (let ((sessions (rex-gt--sessions)))
     (list (if (= 1 (length sessions))
               (cdr (car sessions))
             (cdr (assoc (completing-read "In session: " (mapcar #'car sessions) nil t)
                         sessions)))
           (read-string "Tab label: ")
           (read-directory-name "Tab directory: " default-directory nil t))))
  (rex-gt--run (list "window" "new" label "-s" session "-c" (expand-file-name directory)))
  (condition-case nil
      (rex-gt--client-action "session.select" (cons "session_id" session))
    (error nil))
  (rex-gt-focus-app)
  (message "Created Rex tab `%s' in %s" label directory))

;;;###autoload
(defun rex-gt-capture (&optional pane)
  "Show a Rex pane's screen and scrollback in a read-only buffer.
With no PANE, choose it by name.
Revert the buffer (\\<special-mode-map>\\[revert-buffer]) to capture again."
  (interactive)
  (rex-gt--capture-buffer
   (or pane (rex-gt-read-pane "Capture pane: " nil))))

(defvar rex-gt-capture-pane nil
  "Buffer-local pane captured in a `special-mode' capture buffer.")

(defun rex-gt--capture-buffer (pane)
  "Fill a read-only buffer with PANE's screen and scrollback."
  (let* ((name (format "*rex-capture %s/%s*"
                       (or (alist-get 'session_label pane) "?")
                       (or (alist-get 'title pane)
                           (alist-get 'block_label pane)
                           "?")))
         (buffer (get-buffer-create name))
         (text (rex-gt--run (list "capture" "-s" (alist-get 'session_id pane)
                                  "-b" (alist-get 'block_id pane)
                                  "--trim" "--unwrap"))))
    (with-current-buffer buffer
      (special-mode)
      (setq-local rex-gt-capture-pane pane)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (rex-gt--capture-buffer rex-gt-capture-pane)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun rex-gt--state-face (state)
  "Return the face for an agent STATE reported by a program in a pane."
  (cdr (assoc state
              '(("working" . font-lock-keyword-face)
                ("blocked" . warning)
                ("error" . error)
                ("done" . success)
                ("idle" . shadow)))))

;; ============================================================================
;; Pane manager
;; ============================================================================

;;;###autoload
(defun rex-gt-open-pane-directory (directory &optional label)
  "Show the Rex pane's DIRECTORY in Emacs; the entry point Rex's key calls.
Rex's `ctrl+n>e' binding (`modules.rex.openInEmacsKey') hands over the
working directory of the pane it had focused and that pane's label in
LABEL.  It reaches the Emacs the user is already looking at, so open in
another window instead of taking the frame.

The binding lives in modules/terminal-multiplexer/rex.nix and names
this function, so renaming it means changing both."
  (interactive
   (list (read-directory-name "Rex pane directory: " default-directory nil t)))
  (unless (file-directory-p directory)
    (user-error "No such directory: %s" directory))
  (dired-other-window (file-name-as-directory directory))
  (message "Rex pane %s: %s" (or label directory) directory))

(defvar rex-gt-tab-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'rex-gt-tab-manager-focus)
    (define-key map "c" #'rex-gt-tab-manager-capture)
    (define-key map "d" #'rex-gt-tab-manager-dired)
    (define-key map "D" #'rex-gt-tab-manager-dired-other-window)
    (define-key map "r" #'rex-gt-tab-manager-rename-tab)
    (define-key map "R" #'rex-gt-tab-manager-rename-pane)
    (define-key map "s" #'rex-gt-tab-manager-cycle-sort)
    (define-key map "g" #'rex-gt-tab-manager-refresh)
    map)
  "Keymap for `rex-gt-tab-manager-mode'.")

(defconst rex-gt-tab-manager--sort-cycle
  '(nil ("Agent" . nil) ("Session" . nil) ("Tab" . nil) ("Title" . nil) ("CWD" . nil) ("Cmd" . nil))
  "Order `s' walks `tabulated-list-sort-key' through.
nil keeps the server's own order, with the focused panes first.")

(defvar-local rex-gt-tab-manager--timer nil
  "Fallback poll timer, or nil.")

(defvar-local rex-gt-tab-manager--process nil
  "The fetch currently in flight, or nil.")

(defvar-local rex-gt-tab-manager--events nil
  "The `rex events' process following the server for this table, or nil.")

(defvar-local rex-gt-tab-manager--debounce nil
  "Pending event-triggered refresh, or nil.")

(defun rex-gt--numeric-sort (key)
  "Return a column predicate that sorts entries on numeric plist KEY."
  (lambda (a b)
    (< (or (plist-get (car a) key) most-negative-fixnum)
       (or (plist-get (car b) key) most-negative-fixnum))))

(defun rex-gt-tab-manager--format ()
  "Return `tabulated-list-format' for the pane table."
  `[("Cur" 3 t)
    ("Session" 16 t)
    ("Tab" 12 t)
    ("Title" 24 t)
    ("Agent" 8 t)
    ("App" 9 t)
    ("Cmd" 16 t)
    ("PID" 6 ,(rex-gt--numeric-sort :pid))
    ("CWD" 0 t)])

(defun rex-gt-tab-manager--default-order (panes)
  "Sort PANES so what the user is looking at comes first.
Focused panes of the active window, then any other focused pane, then
the rest in the order the server listed them."
  (seq-sort-by (lambda (pane)
                 (cond ((and (alist-get 'focused pane) (alist-get 'active_window pane)) 0)
                       ((alist-get 'focused pane) 1)
                       (t 2)))
               #'<
               panes))

(defun rex-gt-tab-manager--entries (panes)
  "Build `tabulated-list-entries' from PANES."
  (mapcar (lambda (pane)
            (list (list :pane pane :pid (alist-get 'pid pane))
                  (vector
                   (cond ((and (alist-get 'focused pane) (alist-get 'active_window pane)) "*")
                         ((alist-get 'focused pane) "+")
                         (t ""))
                   (or (alist-get 'session_label pane) "")
                   (or (alist-get 'window_label pane) "")
                   (or (alist-get 'title pane) (alist-get 'block_label pane) "")
                   (if-let* ((state (alist-get 'state pane)))
                       (propertize state 'face (rex-gt--state-face state))
                     "")
                   (or (alist-get 'app pane) "")
                   (if-let* ((cmd (alist-get 'cmd pane)))
                       (file-name-nondirectory cmd)
                     "")
                   (if-let* ((pid (alist-get 'pid pane))) (number-to-string pid) "")
                   (or (alist-get 'cwd pane) ""))))
          (rex-gt-tab-manager--default-order panes)))

(defun rex-gt-tab-manager--pane ()
  "Return the pane at point, or signal a `user-error'."
  (if-let* ((id (tabulated-list-get-id)))
      (or (plist-get id :pane) (user-error "No Rex pane on this line"))
    (user-error "No Rex pane on this line")))

;;;###autoload
(define-derived-mode rex-gt-tab-manager-mode tabulated-list-mode " Rex-Panes"
  "Major mode listing every pane of every Rex session.
One row per pane: `*' marks the pane focused in the session's active
window, `+' the pane focused in another window.  RET shows the pane in
the Rex app, c reads its screen, d / D open Dired on its directory, r
renames its tab, R renames the pane, s cycles the sort key, g
refreshes.  A `rex events' process keeps the table current while it is
on screen; see `rex-gt-refresh-interval' for the fallback poll."
  :group 'rex-gt
  :keymap rex-gt-tab-manager-mode-map
  (setq tabulated-list-format (rex-gt-tab-manager--format)
        tabulated-list-padding 1
        tabulated-list-sort-key nil
        tabulated-list-entries nil)
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'rex-gt-tab-manager--stop-listener nil t)
  (rex-gt-tab-manager--refresh)
  (rex-gt-tab-manager--start-listener)
  (rex-gt-tab-manager--start-timer))

(defun rex-gt-tab-manager-refresh ()
  "Re-read Rex and reprint this table now."
  (interactive)
  (rex-gt-tab-manager--refresh))

(defun rex-gt-tab-manager--print (buffer panes)
  "Reprint BUFFER from PANES; do nothing once BUFFER is gone."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq tabulated-list-entries (rex-gt-tab-manager--entries panes))
      (tabulated-list-print t))))

(defun rex-gt-tab-manager--start-fetch ()
  "Start a background fetch for the table in the current buffer."
  (let ((buffer (current-buffer)))
    (condition-case err
        (setq rex-gt-tab-manager--process
              (rex-gt-gather-async (apply-partially #'rex-gt-tab-manager--print buffer)))
      (file-error
       (setq rex-gt-tab-manager--process nil)
       (message "rex-gt: %s" (error-message-string err))))))

(defun rex-gt-tab-manager--refresh (&optional silent)
  "Fetch Rex's state and reprint the table without blocking.
Stay quiet when SILENT, as the automatic refresh does."
  (if (process-live-p rex-gt-tab-manager--process)
      (unless silent
        (message "rex-gt: refresh already running"))
    (rex-gt-tab-manager--start-fetch)))

(defun rex-gt-tab-manager--start-timer ()
  "(Re)arm the fallback poll for this buffer."
  (when rex-gt-tab-manager--timer
    (cancel-timer rex-gt-tab-manager--timer)
    (setq rex-gt-tab-manager--timer nil))
  (when rex-gt-refresh-interval
    (setq rex-gt-tab-manager--timer
          (run-with-timer rex-gt-refresh-interval rex-gt-refresh-interval
                          (lambda (buffer)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (rex-gt-tab-manager--refresh t))))
                          (current-buffer)))))

(defun rex-gt-tab-manager--schedule-refresh ()
  "Fold a burst of server events into one refresh."
  (unless rex-gt-tab-manager--debounce
    (setq rex-gt-tab-manager--debounce
          (run-with-timer 0.25 nil
                          (lambda (buffer)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (setq rex-gt-tab-manager--debounce nil)
                                (rex-gt-tab-manager--refresh t))))
                          (current-buffer)))))

(defun rex-gt-tab-manager--start-listener ()
  "Follow server events so the table updates as things change.
Only \='something happened' matters here, so the filter never reads what
`rex events' prints, and a buffer nobody is looking at stays put until
the poll or `g' asks for it."
  (when (process-live-p rex-gt-tab-manager--events)
    (delete-process rex-gt-tab-manager--events)
    (setq rex-gt-tab-manager--events nil))
  (let ((buffer (current-buffer))
        (default-directory (expand-file-name "~")))
    (setq rex-gt-tab-manager--events
          (condition-case err
              (make-process
               :name "rex-gt-events"
               :command (list (or (executable-find rex-gt-program) rex-gt-program)
                              "events" "--json")
               :noquery t
               :filter (lambda (_process _text)
                         (when (and (buffer-live-p buffer)
                                    (get-buffer-window buffer 'visible))
                           (with-current-buffer buffer
                             (rex-gt-tab-manager--schedule-refresh))))
               :sentinel
               (lambda (process _event)
                 ;; Silently ignored once `rex-gt-tab-manager--stop-listener'
                 ;; has taken the variable away, which is how a deliberate kill
                 ;; avoids reporting its own death.
                 (when (and (buffer-live-p buffer)
                            (eq process (buffer-local-value 'rex-gt-tab-manager--events buffer)))
                   (message "rex-gt: the event stream ended; the poll takes over (%s)"
                            (process-exit-status process)))))
            (file-error
             (message "rex-gt: no event stream (%s); using the poll only"
                      (error-message-string err))
             nil)))))

(defun rex-gt-tab-manager--stop-listener ()
  "Stop the event stream and both timers of this table."
  (dolist (timer (list rex-gt-tab-manager--timer rex-gt-tab-manager--debounce))
    (when timer (cancel-timer timer)))
  (setq rex-gt-tab-manager--timer nil
        rex-gt-tab-manager--debounce nil)
  ;; Nil first, so the sentinel knows this death was on purpose.
  (let ((process rex-gt-tab-manager--events))
    (setq rex-gt-tab-manager--events nil)
    (when (process-live-p process) (delete-process process))))

;;;###autoload
(defun rex-gt-tab-manager ()
  "List every Rex pane across every session.
RET focuses the pane in the Rex app; see `rex-gt-tab-manager-mode' for
the rest of the keys."
  (interactive)
  (unless (rex-gt-running-p)
    (user-error "No Rex server is reachable"))
  (with-current-buffer (get-buffer-create "*rex-gt-panes*")
    (rex-gt-tab-manager-mode)
    (display-buffer (current-buffer))))

(defun rex-gt-tab-manager-focus ()
  "Show the pane on this line in the Rex app and focus it."
  (interactive)
  (rex-gt-focus-pane (rex-gt-tab-manager--pane)))

(defun rex-gt-tab-manager-capture ()
  "Read the screen and scrollback of the pane on this line."
  (interactive)
  (rex-gt--capture-buffer (rex-gt-tab-manager--pane)))

(defun rex-gt-tab-manager--cwd ()
  "Return the working directory of the pane on this line."
  (let ((cwd (alist-get 'cwd (rex-gt-tab-manager--pane))))
    (or (and cwd (file-directory-p cwd) (file-name-as-directory cwd))
        (user-error "No readable directory for this pane"))))

(defun rex-gt-tab-manager-dired ()
  "Open Dired on the pane's working directory."
  (interactive)
  (dired (rex-gt-tab-manager--cwd)))

(defun rex-gt-tab-manager-dired-other-window ()
  "Open Dired on the pane's working directory in another window."
  (interactive)
  (dired-other-window (rex-gt-tab-manager--cwd)))

(defun rex-gt-tab-manager-rename-tab (label)
  "Rename the tab (window) holding the pane on this line to LABEL."
  (interactive
   (list (let ((pane (rex-gt-tab-manager--pane)))
           (read-string "New tab label: " (alist-get 'window_label pane)))))
  (let ((pane (rex-gt-tab-manager--pane)))
    (rex-gt--run (list "window" "rename" "-s" (alist-get 'session_id pane)
                       (alist-get 'window_id pane) label))
    (message "Renamed tab to `%s'" label)))

(defun rex-gt-tab-manager-rename-pane (label)
  "Rename the pane on this line to LABEL."
  (interactive
   (list (let ((pane (rex-gt-tab-manager--pane)))
           (read-string "New pane label: " (alist-get 'block_label pane)))))
  (let ((pane (rex-gt-tab-manager--pane)))
    (rex-gt--run (list "block" "rename" "-s" (alist-get 'session_id pane)
                       "-w" (alist-get 'window_id pane)
                       (alist-get 'block_id pane) label))
    (message "Renamed pane to `%s'" label)))

(defun rex-gt-tab-manager-cycle-sort ()
  "Cycle the sort key: server order, Agent, Session, Tab, Title, CWD, Cmd."
  (interactive)
  (let* ((keys rex-gt-tab-manager--sort-cycle)
         (rest (member tabulated-list-sort-key keys))
         (next (if (cdr rest) (cadr rest) (car keys))))
    (setq tabulated-list-sort-key next)
    (tabulated-list-print)
    (message "rex-gt: sort by %s" (or (car next) "server order"))))

(provide 'rex-gt)
;;; rex-gt.el ends here
