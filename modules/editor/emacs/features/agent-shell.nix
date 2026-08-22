{ pkgs, ... }:

{
  epkgs = epkgs: [
    epkgs.agent-shell
    pkgs.claude-code-acp
    pkgs.cursor-agent-acp
    pkgs.pi-acp
  ];

  elisp = ''
    (use-package agent-shell
      :commands (agent-shell
                 agent-shell-toggle
                 agent-shell-new-shell
                 agent-shell-switch-buffer
                 agent-shell-send-dwim
                 agent-shell-send-region
                 agent-shell-send-file)
      ;; `C-c a' is already used by org-agenda, so keep agent-shell under
      ;; the separate `C-c A' prefix.
      :bind (("C-c A a" . agent-shell)
             ("C-c A n" . agent-shell-new-shell)
             ("C-c A b" . agent-shell-switch-buffer)
             ("C-c A s" . agent-shell-send-dwim))
      :init
      (setq agent-shell-preferred-agent-config 'pi))

    (use-package agent-shell-pi
      :after agent-shell
      :commands (agent-shell-pi-start-agent)
      :bind (("C-c A p" . agent-shell-pi-start-agent))
      :init
      (setq agent-shell-pi-acp-command '("${pkgs.pi-acp}/bin/pi-acp")))

    (use-package agent-shell-anthropic
      :after agent-shell
      :commands (agent-shell-anthropic-start-claude-code)
      :bind (("C-c A c" . agent-shell-anthropic-start-claude-code))
      :init
      (setq agent-shell-anthropic-claude-acp-command
            '("${pkgs.claude-code-acp}/bin/claude-code-acp")))

    (use-package agent-shell-cursor
      :after agent-shell
      :commands (agent-shell-cursor-start-agent)
      :bind (("C-c A C" . agent-shell-cursor-start-agent))
      :init
      (setq agent-shell-cursor-acp-command
            '("${pkgs.cursor-agent-acp}/bin/cursor-agent-acp")))
  '';
}
