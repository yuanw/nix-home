;;; tempel.el --- Tempel completion + pi session openers -*- lexical-binding: t; -*-

;;; Commentary:

;; Installs and configures Tempel (minad/tempel) as the completion-at-point
;; template engine, and contributes two "pi session openers": fillable
;; skeletons you can drop either into a buffer (via Tempel) or straight into
;; the pi agent-shell prompt (via `agent-shell-insert').
;;
;; The two shapes, from the pi-sessions week review
;; (/20260919T084509--pi-sessions-week-review-prompt-steering__llm.org/):
;;   - policy   : you already know goal / constraints / done-when.
;;   - diagnose : you are NOT sure yet -> map options, then STOP.
;;
;; The opener text lives in ONE place (`my-pi-openers-policy-text' /
;; `my-pi-openers-diagnose-text', both customizable); both the Tempel
;; templates and the agent-shell commands read those same variables -- no
;; duplicated strings.  The agent-shell package itself is provided by
;; features/agent-shell.nix.

;;; Code:

;; --- shared data: single source of truth (top-level, so the agent-shell
;; --- commands below never observe a void value, even before Tempel loads) --
(defcustom my-pi-openers-policy-text
  "Goal: <make `just build`/`just switch` green on <host>>
Context: <link the known cause / prior note / PR / URL>
Constraints: <don't touch <sacred file>; prefer the <pattern>; draft PR only>
Done when: <`just colmena-spark-build` passes AND `curl <vllm>/v1/models` lists the model>
Verify with: <nix eval | emacsclient eval | monitor-ci | browser-cli on <URL>>"
  "Policy (known-plan) opener: front-load goal / constraints / done-when."
  :group 'pi-openers :type 'string)

(defcustom my-pi-openers-diagnose-text
  "Symptom / goal: <what is broken / what you want>
What I know: <URL, error snippet, related PR, doc>
What I don't know yet: <approach, how far to go, draft vs ready>
Please:
  1. Diagnose (short).
  2. List 2-3 options with tradeoffs and a recommendation.
  3. Stop. Do not edit, commit, or open a PR until I pick.
Hard limits (if any): <e.g. draft-only; don't touch <repo Y>; 30 min then report>"
  "Diagnose (map-options-first) opener: diagnose -> options -> you choose -> execute."
  :group 'pi-openers :type 'string)

(defun my-pi-openers--tempel-templates ()
  "Contribute the pi openers to Tempel from the shared text variables.
This is a `tempel-template-sources' function source, so it always reflects
the current `my-pi-openers-*-text' customizations."
  `((pi-policy   ,my-pi-openers-policy-text
      :ann "agent opener: policy known")
    (pi-diagnose ,my-pi-openers-diagnose-text
      :ann "agent opener: diagnose first")))

(use-package tempel
  :commands (tempel-insert tempel-expand tempel-complete)
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.  `tempel-expand'
    ;; only triggers on exact matches. We add `tempel-expand' *before* the main
    ;; programming mode Capf, such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions))

    ;; Alternatively use `tempel-complete' if you want to see all matches.  Use
    ;; a trigger prefix character in order to prevent Tempel from triggering
    ;; unexpectly.
    ;; (setq-local corfu-auto-trigger "/"
    ;;             completion-at-point-functions
    ;;             (cons (cape-capf-trigger #'tempel-complete ?/)
    ;;                   completion-at-point-functions))
    )

  :config

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Register the pi openers as a Tempel template source and bind them.
  ;; (add-to-list '<place-variable> '<element>): push the source function symbol
  ;; onto tempel's `tempel-template-sources' -- house idiom per magit.el/completion.el.
  (add-to-list 'tempel-template-sources 'my-pi-openers--tempel-templates)
  ;; `tempel-key' is the documented binding macro (generates an autoloaded
  ;; `tempel-insert-<name>' command).  `C-c q' is a verified-free prefix.
  (tempel-key "C-c q p" pi-policy)
  (tempel-key "C-c q d" pi-diagnose)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; --- agent-shell path: drop the opener into the pi prompt -----------------
;; `agent-shell-insert' contract (agent-shell.el):
;;   (&key text submit no-focus shell-buffer)  -- inserts TEXT at prompt-max,
;;   optionally SUBMITs it.
(defun my-pi-openers-agent-policy ()
  "Insert the policy opener into the agent-shell prompt (edit, then send)."
  (interactive)
  (agent-shell-insert :text my-pi-openers-policy-text))

(defun my-pi-openers-agent-policy-send ()
  "Insert and submit the policy opener to the pi agent."
  (interactive)
  (agent-shell-insert :text my-pi-openers-policy-text :submit t))

(defun my-pi-openers-agent-diagnose ()
  "Insert the diagnose opener into the agent-shell prompt (edit, then send)."
  (interactive)
  (agent-shell-insert :text my-pi-openers-diagnose-text))

(defun my-pi-openers-agent-diagnose-send ()
  "Insert and submit the diagnose opener to the pi agent."
  (interactive)
  (agent-shell-insert :text my-pi-openers-diagnose-text :submit t))

;; Bind the agent-shell variants under the same `C-c q' prefix (a = agent).
;; `keymap-global-set' (KEY FUNC) is the repo-proven idiom from
;; features/completion.el; multi-key sequences create the prefix map itself.
(keymap-global-set "C-c q a p" 'my-pi-openers-agent-policy)
(keymap-global-set "C-c q a P" 'my-pi-openers-agent-policy-send)
(keymap-global-set "C-c q a d" 'my-pi-openers-agent-diagnose)
(keymap-global-set "C-c q a D" 'my-pi-openers-agent-diagnose-send)

(provide 'nima-feature-tempel)
;;; tempel.el ends here
