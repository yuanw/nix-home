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

      :init
      (setq agent-shell-preferred-agent-config 'pi))

    (use-package agent-shell-pi
      :after agent-shell
      :commands (agent-shell-pi-start-agent)
      :bind (("C-c a p" . agent-shell-pi-start-agent))
      :init
      (setq agent-shell-pi-acp-command '("${pkgs.pi-acp}/bin/pi-acp")))

  '';
}
