{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.podman;
in
{
  options.modules.dev.podman = {
    enable = mkEnableOption "podman dev environment with Docker compatibility";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        podman
        podman-compose
      ];

      # Testcontainers reads this file to override defaults.
      # We set ryuk.disabled here (static); DOCKER_HOST is set dynamically
      # in the shell because the Podman socket path contains $TMPDIR which
      # is only known at runtime.
      home.file.".testcontainers.properties".text = ''
        # Ryuk requires privileged Docker socket access unavailable under
        # rootless Podman. Disable it so Testcontainers starts cleanly.
        testcontainers.ryuk.disabled=true
      '';

      programs.zsh = {
        shellAliases = {
          # podman-compose as a drop-in for the legacy hyphenated form
          docker-compose = "podman-compose";
        };
        # envExtra writes to .zshenv, which is sourced for every zsh invocation
        # (interactive, non-interactive, and login shells alike).  This ensures
        # DOCKER_HOST is set even for the shell that first spawns the Gradle daemon.
        envExtra = ''
          # Point DOCKER_HOST at the Podman socket so tools like Testcontainers
          # work without per-project configuration.
          # Checked at shell startup so it reflects the currently running machine.
          if [[ -z "$DOCKER_HOST" ]]; then
            if [[ -n "$TMPDIR" && -S "''${TMPDIR%/}/podman/podman-machine-default-api.sock" ]]; then
              export DOCKER_HOST="unix://''${TMPDIR%/}/podman/podman-machine-default-api.sock"
            elif [[ -n "$XDG_RUNTIME_DIR" && -S "$XDG_RUNTIME_DIR/podman/podman.sock" ]]; then
              export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
            fi
          fi
        '';
        initContent = ''
          # Route all `docker` calls to podman.
          # `docker compose` (plugin form) is intercepted and forwarded to podman-compose,
          # since shell aliases cannot intercept subcommands.
          function docker() {
            if [[ "$1" == "compose" ]]; then
              shift
              podman-compose "$@"
            else
              podman "$@"
            fi
          }
        '';
      };
    };
  };
}
