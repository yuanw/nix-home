# User-level ~/.config/nix/nix.conf via Home Manager (modules/misc/nix).
# System/daemon settings stay in modules/common.nix → /etc/nix/nix.conf.
# Nix merges both; only put extras here that are not in common.nix.
{
  lib,
  pkgs,
  ...
}:
{
  nix = {
    package = lib.mkDefault pkgs.nix;

    settings = {
      substituters = [
        "https://cache.garnix.io"
        "https://cache.zw3rk.com"
      ];
      trusted-public-keys = [
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
      ];
    };
  };
}
