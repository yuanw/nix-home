{ config, lib, ... }:
let
  cfg = config.services.harmonia-cache;
  # The nixpkgs module derives the systemd socket's ListenStream from
  # settings.bind, so the firewall port below and bind here are one number.
  port = 5000;
in
{
  options.services.harmonia-cache = {
    enable = lib.mkEnableOption "LAN Nix binary cache served from this host's /nix/store";
  };

  config = lib.mkIf cfg.enable {
    # root:root 0400 is agenix's default and what LoadCredential wants; the
    # service is DynamicUser, so never pin owner/group to a static user.
    age.secrets.harmonia-sign-key.file = ../../../secrets/harmonia-sign-key.age;

    services.harmonia = {
      cache = {
        enable = true;
        signKeyPaths = [ config.age.secrets.harmonia-sign-key.path ];
        settings = {
          bind = "[::]:${toString port}";
          # Below cache.nixos.org's 40 so the LAN wins when both have the path.
          priority = 10;
        };
      };
      # Replaces nix.gc on this host; nixpkgs warns if both are automatic.
      gc = {
        enable = true;
        automatic = true;
        dates = [ "weekly" ];
        deleteOlderThan = "30d";
        ensureFree = "50G";
        keepRecent = "1d";
        # No gcRootsDirs: Phase 3.4's roots go under /nix/var/nix/gcroots/cache,
        # which the standard gcroots scan already covers.
      };
    };

    networking.firewall.allowedTCPPorts = [ port ];

    # The cache has nothing to serve that GC already collected.
    nix.settings = {
      keep-derivations = true;
      keep-outputs = true;
    };
  };
}
