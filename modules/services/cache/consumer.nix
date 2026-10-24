# A host that substitutes from silver's Harmonia cache — Phase 2 of
# docs/harmonia-build-hub.org.
#
# Imported from two directions:
#
# - directly, by the hosts that never import myModules.common (misfit,
#   dgx-spark), whose daemon configs are their own;
# - through modules/nix-custom-conf.nix, which mkForce's whole substituter
#   lists and therefore reads the option values below instead of relying on the
#   additions in this file's config (a mkForce would silently drop them).
#
# The url and the key are one unit: require-sigs rejects a substituter whose
# signing key is missing, so anything that adds the url must add the key.
# Rotating the key means re-encrypting secrets/harmonia-sign-key.age and
# redeploying every consumer — see Phase 1.1 of the plan.
{ config, lib, ... }:
let
  cfg = config.my.silver-cache;
in
{
  options.my.silver-cache = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Substitute from silver. Turn this off on hosts that leave the LAN:
        silver has no tailscale, so a traveling consumer pays connect-timeout
        on every path the public caches do not have.
      '';
    };

    url = lib.mkOption {
      type = lib.types.str;
      default = "http://silver.lan:5000";
      description = "Harmonia endpoint. Plain HTTP, LAN only — it has no auth and serves any path in silver's store.";
    };

    publicKey = lib.mkOption {
      type = lib.types.str;
      default = "silver-1:bdoSIDAWVU3lhL+8T7TrWXcglLi6oGFlFlZAR0iFVQg=";
      description = "Public half of the cache signing key; clients pin this name.";
    };
  };

  config = lib.mkIf cfg.enable {
    nix.settings = {
      substituters = [ cfg.url ];
      trusted-public-keys = [ cfg.publicKey ];
      # A powered-off silver must not stall every missing path for a minute.
      connect-timeout = 5;
    };
  };
}
