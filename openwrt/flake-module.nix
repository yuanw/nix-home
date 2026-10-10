{ ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      # One buildable target per router: running it prints `uci batch` commands for
      # the declared config.  `just openwrt-diff` pipes that into the router.
      packages.openwrt-e8450 =
        let
          uci = import ./default.nix { inherit pkgs; };
        in
        (uci.writeUci ./routers/e8450.nix).command;
    };
}
