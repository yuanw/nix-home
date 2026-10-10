{
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.agenix.nixosModules.default
    inputs.declarative-jellyfin.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    ../../modules/isponsorblocktv.nix
    ../../modules/caddy.nix
    ./configuration.nix
  ];

  # colmena deployment configuration
  deployment = {
    # `misfit.local` is dead: misfit never answers for its own .local name
    # (avahi-resolve on misfit itself times out), so ssh-ng dies with
    # "stream ended unexpectedly" and the build dangles.  Use the DHCP name,
    # which is what `ssh misfit` resolves to.
    targetHost = "misfit";
    targetUser = "yuan";
    # This control box is aarch64-darwin and has no x86_64-linux builder, so
    # build the closure on misfit itself.
    buildOnTarget = true;
  };

  # declarative-jellyfin does not yet support jellyfin 12.x
  # (https://github.com/Sveske-Juice/declarative-jellyfin/issues/32,
  #  PR #34 in progress) — pin jellyfin & friends to the last supported
  # release from nixos-26.05
  nixpkgs.overlays = [
    (_final: prev: {
      inherit (inputs.nixpkgs-stable.legacyPackages.${prev.system})
        jellyfin
        jellyfin-web
        jellyfin-ffmpeg
        ;
    })
  ];
}
