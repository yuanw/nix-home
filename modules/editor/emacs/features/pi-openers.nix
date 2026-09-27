{ ... }:

# Pi session "openers": fillable skeletons you drop either into a buffer (via
# Tempel) or straight into the pi agent-shell prompt (via `agent-shell-insert').
# Two shapes, from the sessions review
# (/20260919T084509--pi-sessions-week-review-prompt-steering__llm.org/):
#   - policy   : you already know goal / constraints / done-when.
#   - diagnose : you are NOT sure yet -> map options, then STOP.
#
# The opener text lives in ONE place (`my-pi-openers-policy-text' /
# `my-pi-openers-diagnose-text'); both the Tempel templates and the
# agent-shell commands read those same variables -- no duplicated strings.
# The packages themselves are provided by features/tempel.nix and
# features/agent-shell.nix; here we only contribute data + bindings.
{
  elisp = ''
    ;; --- single source of truth: the two opener skeletons -------------
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

    ;; --- in-buffer path: expose the same text as Tempel templates -----
    (use-package tempel
      :commands (tempel-insert tempel-expand tempel-complete)
      :config
      ;; A function source keeps the two surfaces in sync with the custom vars.
      (defun my-pi-openers--tempel-templates ()
        "Contribute the openers to Tempel from the shared text variables."
        `((pi-policy   ,my-pi-openers-policy-text
            :ann "agent opener: policy known")
          (pi-diagnose ,my-pi-openers-diagnose-text
            :ann "agent opener: diagnose first")))
      (add-to-list 'my-pi-openers--tempel-templates tempel-template-sources)
      ;; `tempel-key' is the documented binding macro (generates autoloaded
      ;; `tempel-insert-<name>' commands).  `C-c q' is a verified-free prefix.
      (tempel-key "C-c q p" pi-policy)
      (tempel-key "C-c q d" pi-diagnose))

    ;; --- agent-shell path: drop the opener into the pi prompt ----------
    ;; `agent-shell-insert' contract (agent-shell.el):
    ;;   (&key text submit no-focus shell-buffer)  -- inserts TEXT at prompt-max,
    ;;   optionally SUBMITs it.  We require agent-shell loaded so its commands
    ;;   are defined before our command bodies call them.
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
  '';
}
