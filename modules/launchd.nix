# modules/launchd.nix
#
# nix-darwin owns the launchd user agents its configuration declares: it copies
# the plists into ~/Library/LaunchAgents, loads them with `load -w', and reloads
# one when its content changes. An agent removed from the configuration is on
# its own, though -- the stale-agent pass nix-darwin emits is best-effort (it
# left three behind here), it never kills a KeepAlive process it merely stops
# managing, and a plist that survives returns at the next login.
#
# So leftovers are named here and torn down during activation. Which agents a
# module owns stays that module's business: see modules/wm/yabai.nix.
{ config, lib, ... }:

let
  username = config.my.username;
  cfg = config.modules.launchd;
in
{
  options.modules.launchd.userAgentsToRemove = lib.mkOption {
    type = with lib.types; listOf str;
    default = [ ];
    example = [ "org.nixos.yabai" ];
    description = "launchd user agent labels to boot out and delete at activation.";
  };

  config = lib.mkIf (cfg.userAgentsToRemove != [ ]) {
    system.activationScripts.postActivation.text = lib.mkAfter (
      lib.concatMapStringsSep "\n" (agent: ''
        launchctl bootout "gui/$(id -u '${username}')/${agent}" || true
        rm -f ~${username}/Library/LaunchAgents/${agent}.plist
      '') cfg.userAgentsToRemove
    );
  };
}
