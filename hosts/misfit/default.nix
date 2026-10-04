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
    targetHost = "misfit.local";
    targetUser = "yuan";
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
