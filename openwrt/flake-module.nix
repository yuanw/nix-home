{ ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      uci = import ./default.nix { inherit pkgs; };
      e8450 = uci.writeUci ./routers/e8450.nix;
    in
    {
      # One buildable target per router: running it prints `uci batch` commands for
      # the declared config.  `just openwrt-diff` pipes that into the router.
      packages.openwrt-e8450 = e8450.command;

      # Nothing else runs the 189 lines of serialization logic, so it is easy to edit
      # this directory and ship it broken. Both checks fail here rather than on the
      # router, which is the only place the failure has ever been observed.
      checks.openwrt-serialization = pkgs.runCommand "openwrt-serialization" {
        nativeBuildInputs = [ pkgs.python3 ];
      } ''
        python3 ${./.}/selfcheck.py > $out
      '';

      # A config that does not evaluate is caught here; `just openwrt-eval` would only
      # notice halfway through reading the wifi key out of pass.
      checks.openwrt-e8450-eval = pkgs.runCommand "openwrt-e8450-eval" { }
        ''
          secrets=$(mktemp)
          printf '{"wireless_key": "check-only"}' > "$secrets"
          NIX_UCI_SECRETS="$secrets" ${e8450.command} | grep -q '^set '
          touch $out
        '';
    };
}
