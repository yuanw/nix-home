{
  inputs,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    inputs.agenix.nixosModules.default
    inputs.jellarr.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    ../../modules/isponsorblocktv.nix
    ../../modules/healthsave-observatory.nix
    ../../modules/caddy.nix
    # misfit never imports myModules.common, so the LAN cache consumer has to be
    # imported here (docs/harmonia-build-hub.org Phase 2, finding 3).
    ../../modules/services/cache/consumer.nix
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

  # Jellyfin is pinned to 10.11.x. Jellarr no longer forces it (it only speaks
  # the REST API, so unlike declarative-jellyfin it never cared which internal
  # schema it was writing), but the Open Subtitles plugin installed in
  # ./configuration.nix is the 24.0.0.0 build for the 10.11 ABI — 25.x targets
  # jellyfin 12 and failed to load the way TheTVDB did. Unpin together with a
  # plugin bump, in one change, and re-check the two meta.json targetAbi values.
  nixpkgs.overlays = [
    (_final: prev: {
      inherit (inputs.nixpkgs-stable.legacyPackages.${prev.stdenv.hostPlatform.system})
        jellyfin
        jellyfin-web
        jellyfin-ffmpeg
        ;
    })
    # jellarr's nix/package.nix pins `pnpmDeps` with fetcherVersion 4 and a hash
    # resolved against pnpm 11.15 (the pnpm in its own flake lock); misfit carries
    # pnpm 12.9.0, whose resolution tree hashes differently, so the declared hash
    # fails the fixed-output check. Measured on silver 2026-11-14:
    #   specified sha256-jo1BjRAjjfNKF0xb5cLCuELSveHeJ98iLPhMDKP1QbI=
    #   got       sha256-qNVnhHjTFPhJxJ8oZPBSfJs2OjNSlbmS31okZuSGWMU=
    # Setting `inputs.jellarr.inputs.nixpkgs.follows = "nixpkgs"` fixes nothing —
    # its module builds the package with `pkgs.callPackage ../package.nix {}`,
    # i.e. always against *our* pnpm — and the hash is an argument, so the
    # fetcher is the only seam left.
    #
    # shortcut: swaps the hash for pname = "jellarr" only and hands every other
    # call straight through, but the wrapper drops fetchPnpmDeps's `.override`/
    # `.version`, so if a second pnpm-based package lands on misfit and wants
    # those, move this to a forked package.nix instead. Drop it when jellarr
    # rehashes for pnpm 12 upstream.
    (_final: prev: {
      fetchPnpmDeps =
        args:
        prev.fetchPnpmDeps (
          if (args.pname or "") == "jellarr" then
            args // { hash = "sha256-qNVnhHjTFPhJxJ8oZPBSfJs2OjNSlbmS31okZuSGWMU="; }
          else
            args
        );
    })
  ];
}
