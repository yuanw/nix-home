# direnv's `use flake` through iets (github:numtide/iets).
#
# A warm load reads iets's own tracing cache and never asks the daemon, and the
# watches cover every file the evaluation read, not only flake.nix and
# flake.lock: https://github.com/numtide/iets/blob/main/docs/how-to/use-with-direnv.md
#
# This is the home-manager twin of iets's nixosModules.default, which cannot be
# imported here: it is a NixOS module, so it defines `iets.direnv.enable` (the
# option this file declares) and writes `environment.systemPackages`,
# `nix.package` and `programs.direnv.direnvrcExtra` -- none of which exist in
# home-manager. Measured, not assumed. The .envrc still says `use flake`.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  cfg = config.iets.direnv;
  system = pkgs.stdenv.hostPlatform.system;
in
{
  options.iets.direnv.enable = lib.mkEnableOption "iets's use flake in direnv, in place of nix-direnv's";

  config = lib.mkIf cfg.enable {
    home.packages = [ inputs.iets.packages.${system}.default ];

    # direnv reads lib/*.sh in name order, so this lands after hm-nix-direnv.sh
    # and its use_flake wins -- the same trick the nixos module plays with
    # direnvrcExtra. nix-direnv stays enabled for `use nix`.
    xdg.configFile."direnv/lib/zz-iets.sh".text = ''
      use_flake() {
        local ref="''${1:-.}" uri root entry script
        local -a paths
        if ! has iets; then
          log_error "use flake: iets is not installed"
          return 1
        fi
        uri="''${ref%#*}"
        uri="''${uri#path:}"
        if [[ ! -f $uri/flake.lock ]]; then
          log_error "use flake: $uri has no flake.lock (run nix flake lock)"
          return 1
        fi
        # iets resolves <system> from the system its own binary was built for, and
        # every build claims x86_64-linux (src/eval/mod.rs DEFAULT_SYSTEM), so a
        # bare ref asks a darwin host for a linux devShell. Name the system until
        # upstream fixes it; an explicit #attr from the .envrc is left alone.
        [[ $ref == *#* ]] || ref="$ref#devShells.${system}.default"
        root="$(direnv_layout_dir)/iets-env"
        entry="$(direnv_layout_dir)/iets-entry"
        mkdir -p "$(direnv_layout_dir)"
        watch_file "$uri/flake.nix" "$uri/flake.lock"
        # --gc-root needs the local daemon; --entry-to names the cache entry this
        # run read, so the watch list below needs no second key derivation.
        script="$(iets print-dev-env --builder daemon --gc-root "$root" --entry-to "$entry" "$ref")" || true
        # before the eval of the environment, so a shell hook that fails cannot
        # lose the watches, and in one call, because watch_file forks direnv
        # every time (25 ms for 500 paths in one call, 7.3 s one at a time).
        if [[ -s $entry && -e $(<"$entry") ]]; then
          mapfile -d "" -t paths < <(iets cache paths "$(<"$entry")" 2>/dev/null)
          ((''${#paths[@]})) && watch_file "''${paths[@]}"
        fi
        eval "$script"
      }
    '';
  };
}
