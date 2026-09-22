{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.modules.dev.podman;
  # Docker Compose v2 speaks the CLI wk-local expects (`ps -a --format json`);
  # podman-compose does not. Point podman's `compose` subcommand at it.
  composeProvider = "${pkgs.docker-compose}/bin/docker-compose";
in
{
  options.modules.dev.podman = {
    enable = mkEnableOption "podman dev environment with Docker compatibility";
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = with pkgs; [
        podman
        docker-compose
        # Real binary on PATH (not a zsh function) so Go's exec.Command("docker")
        # — e.g. wk-local — finds it. Matches NixOS virtualisation.podman.dockerCompat.
        (runCommand "docker-podman-compat" { } ''
          mkdir -p $out/bin
          ln -s ${podman}/bin/podman $out/bin/docker
        '')
      ];

      home.sessionVariables = {
        PODMAN_COMPOSE_PROVIDER = composeProvider;
        # Ryuk needs a privileged Docker socket unavailable under rootless Podman.
        TESTCONTAINERS_RYUK_DISABLED = "true";
      };

      programs.zsh = {
        # envExtra → .zshenv (every zsh). DOCKER_HOST is dynamic on the running machine socket.
        envExtra = ''
          export PODMAN_COMPOSE_PROVIDER="${composeProvider}"
          export TESTCONTAINERS_RYUK_DISABLED=true
          if [[ -z "$DOCKER_HOST" ]]; then
            if [[ -n "$TMPDIR" && -S "''${TMPDIR%/}/podman/podman-machine-default-api.sock" ]]; then
              export DOCKER_HOST="unix://''${TMPDIR%/}/podman/podman-machine-default-api.sock"
            elif [[ -n "$XDG_RUNTIME_DIR" && -S "$XDG_RUNTIME_DIR/podman/podman.sock" ]]; then
              export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
            fi
          fi
        '';
      };
    };
  };
}
