{
  pkgs,
  nixConfig,
  lib,
  ...
}:

# Antigravity's `agy` binary comes from the antigravity-cli module (gated on
# `modules.antigravity-cli.enable`); follow the spookfox.nix pattern of reading
# that through `nixConfig` and only wiring the feature up where it exists.
let
  enableAntigravity = lib.attrByPath [ "modules" "antigravity-cli" "enable" ] false nixConfig;
  # Only referenced (and only forced) inside the interpolated block below, so
  # hosts without the antigravity-cli module never evaluate the package.
  agy = lib.attrByPath [ "modules" "antigravity-cli" "package" ] null nixConfig;
in
{
  # Do not `(require)' these at startup: hel-collection's agent-shell mode file
  # defines its leader keymaps after the `with-eval-after-load' that binds them,
  # so loading that file with agent-shell already loaded dies on
  # `hel-agent-shell-local-leader-map'.
  requireEpkgs = false;

  epkgs = epkgs: [
    epkgs.agent-shell
    epkgs.agent-shell-knockknock
    epkgs.agent-shell-manager
    epkgs.knockknock
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
                 agent-shell-send-file
                 ;; Not in the package's own autoloads, and tempel.el calls it.
                 agent-shell-insert)
      :init
      (setq agent-shell-preferred-agent-config 'pi))

    (use-package agent-shell-pi
      :after agent-shell
      :commands (agent-shell-pi-start-agent)
      :init
      (setq agent-shell-pi-acp-command '("${pkgs.pi-acp}/bin/pi-acp")))

    (use-package agent-shell-knockknock
      :after (agent-shell knockknock)
      :hook (agent-shell-mode . agent-shell-knockknock-mode))

    (use-package agent-shell-manager
      :after agent-shell
      :commands (agent-shell-manager-toggle))

    ${lib.optionalString enableAntigravity ''
      (use-package agent-shell-antigravity
        :after agent-shell
        :commands (agent-shell-antigravity-start-agent)
        :init
        (setq agent-shell-antigravity-acp-command '("${agy}/bin/agy" "acp")))
    ''}
  '';
}
