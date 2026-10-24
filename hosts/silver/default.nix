{ inputs, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.apple-macbook-pro-11-5
    inputs.disko.nixosModules.disko
    inputs.agenix.nixosModules.default
    ../../modules/cockpit.nix
    ../../modules/services/cache/harmonia.nix
    ../../modules/services/cache/push.nix
    ../../modules/services/monitoring/pcp.nix
    ./disk-config.nix
    ./configuration.nix
  ];

  # The repo's own packages, notably pcp (re-added here because upstream nixpkgs
  # removed it). Without this, services.pcp resolves pkgs.pcp and throws
  # "'pcp' has been removed".
  nixpkgs.overlays = [ (import ../../packages) ];

  # colmena deployment configuration
  deployment = {
    # "silver.local" needs mDNS, which is not reliable here (another box on the
    # LAN also calls itself silver, so the name conflict makes the lookup fail or
    # land wrong). Plain "silver" resolves through the router's DNS as silver.lan.
    targetHost = "silver";
    targetUser = "yuanw";
    # No x86_64-linux builder on the aarch64-darwin control box, so build the
    # closure on the target. Also how the private flake input gets fetched there
    # (the forwarded ssh-agent), same as dgx-spark.
    buildOnTarget = true;
  };
}
