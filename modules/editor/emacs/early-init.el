;; Disable some GUI distractions. We set these manually to avoid starting
;; the corresponding minor modes.
(dolist (parameter '((menu-bar-lines . 0)
                     (tool-bar-lines . nil)
                     (vertical-scroll-bars . nil)
                     (undecorated-round . t)))
  (add-to-list 'default-frame-alist parameter)
  (add-to-list 'initial-frame-alist parameter))

;; Set up fonts early.
;; `my-mono-font' and `my-font' are defined from Nix in default.nix.
;;--------------------
(set-face-attribute 'default nil :family my-mono-font :height 180)
(set-face-attribute 'fixed-pitch nil :family my-mono-font :height 1.0)
(set-face-attribute 'variable-pitch nil :family my-font :height 1.0)

;; auto-save might handle this already
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Disable startup message.
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))
(setq-default inhibit-startup-screen t)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Luminosity 50
(defconst palette-red               "#FF0000")
(defconst palette-orange            "#FF8000")
(defconst palette-yellow            "#FFFF00")
(defconst palette-yellow-green      "#80FF00")
(defconst palette-green             "#00FF00")
(defconst palette-teal              "#00FF80")
(defconst palette-cyan              "#00FFFF")
(defconst palette-slate-blue        "#007FFF")
(defconst palette-blue              "#0000FF")
(defconst palette-indigo            "#7F00FF")
(defconst palette-purple            "#FF00FF")
(defconst palette-fuschia           "#FF0080")

;; Luminosity 20
(defconst palette-red-dark          "#660000")
(defconst palette-orange-dark       "#663300")
(defconst palette-yellow-dark       "#666600")
(defconst palette-yellow-green-dark "#336600")
(defconst palette-green-dark        "#006600")
(defconst palette-teal-dark         "#006633")
(defconst palette-cyan-dark         "#006666")
(defconst palette-slate-blue-dark   "#003366")
(defconst palette-blue-dark         "#000066")
(defconst palette-indigo-dark       "#330066")
(defconst palette-purple-dark       "#660066")
(defconst palette-fuschia-dark      "#660033")

;; Luminosity 15
(defconst palette-red-darker          "#4D0000")
(defconst palette-orange-darker       "#4D2600")
(defconst palette-yellow-darker       "#4D4D00")
(defconst palette-yellow-green-darker "#264D00")
(defconst palette-green-darker        "#004D00")
(defconst palette-teal-darker         "#004D26")
(defconst palette-cyan-darker         "#004D4D")
(defconst palette-slate-blue-darker   "#00264D")
(defconst palette-blue-darker         "#00004D")
(defconst palette-indigo-darker       "#26004D")
(defconst palette-purple-darker       "#4D004D")
(defconst palette-fuschia-darker      "#4D0026")

;; Luminosity 10
(defconst palette-red-darkest          "#330000")
(defconst palette-orange-darkest       "#331A00")
(defconst palette-yellow-darkest       "#333300")
(defconst palette-yellow-green-darkest "#1A3300")
(defconst palette-green-darkest        "#003300")
(defconst palette-teal-darkest         "#00331A")
(defconst palette-cyan-darkest         "#003333")
(defconst palette-slate-blue-darkest   "#001A33")
(defconst palentte-blue-darkest        "#000033")
(defconst palette-indigo-darkest       "#1A0033")
(defconst palette-purple-darkest       "#330033")
(defconst palette-fuschia-darkest      "#33001A")

;; Make customisations that affect Emacs faces BEFORE loading a theme
;; (any change needs a theme re-load to take effect).

;; Fix for Emacs 31: prefer .el files over potentially incompatible .elc files
(setq load-prefer-newer t)


(setq org-startup-with-inline-images t)

;; Theme loading moved to init (after display initialization)
;; to avoid color-name-to-rgb issues in early-init

(eval-and-compile
  (mapc (lambda (entry)
          (define-prefix-command (cdr entry))
          (bind-key (car entry) (cdr entry)))
        '(("C-'"   . my-ctrl-quote-map)
          ("<C-m>" . my-ctrl-m-map)
          ("C-c m" . my-ctrl-c-m-map)
          ("C-c n" . my-ctrl-c-n-map)
          ("C-c t" . my-multi-term-map)
          ("C-c w" . my-web-map)
          ("C-c y" . my-yasnippet-map)
          ("C-c H" . my-highlight-map)
          ("C-c N" . my-ctrl-c-N-map))))'

(setq whitespace-style '(face tabs tab-mark spaces space-mark))
;; disable bidirectional text scan
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
