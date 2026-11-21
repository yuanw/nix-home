# Configarr — the TRaSH-Guides syncer for Radarr/Sonarr (custom formats, quality
# definitions, quality profiles).  Planned in
# nix-home-private/docs/misfit-arr.org [[#configarr]]; it is *not* in nixpkgs, so
# this file is the whole reason a configarr derivation exists here.
#
# Upstream distributes per-platform `bun build --compile` executables (see its
# .github/workflows/cli-release.yml) and a Docker image; there is nothing here
# to compile, only a binary to place and a loader to point at it.
#
# THE ONE NON-OBVIOUS THING: do not patchelf it.  autoPatchelfHook rewrites the
# ELF, and a Bun single-file executable carries its JavaScript payload appended
# after the ELF image — patching loses it and the binary silently degrades into
# plain `bun`, printing Bun's own usage text and exiting 0.  Measured on misfit:
# the patched build prints "Bun is a fast JavaScript runtime…" for any argument,
# while the same untouched tarball run through glibc's loader logs
# "Configarr Version: v1.39.2".  So the install phase keeps the byte-identical
# binary under libexec/ and ships a wrapper that invokes the loader explicitly —
# which is also why no PT_INTERP rewrite is needed on a system without /lib64.
#
# Two more things the unit in hosts/misfit/configuration.nix has to carry because
# the binary cannot: `git` on PATH (it clones TRaSH-Guides and the recyclarr
# template repo with simple-git, i.e. it shells out — the strings carry "Failed
# to spawn git process"), and a writable working directory (src/env.ts puts its
# cloned repos at $ROOT_PATH/repos, ROOT_PATH defaulting to the cwd).  It takes
# no CLI flags at all: every knob is an environment variable (CONFIG_LOCATION,
# SECRETS_LOCATION, DRY_RUN, LOG_LEVEL — read off src/env.ts at v1.39.2).
{
  lib,
  stdenv,
  fetchurl,
  glibc,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "configarr";
  version = "1.39.2";

  src = fetchurl {
    url = "https://github.com/raydak-labs/configarr/releases/download/v${finalAttrs.version}/configarr-linux-x64.tar.xz";
    hash = "sha256-4BzDvgZ9HkOMG0sTx9l52gJUONdHr7yL9Aic4FZmjvE=";
  };

  # The archive holds two loose files and no directory, which the generic
  # unpacker refuses ("unpacker appears to have produced no directories").
  # Measured on misfit, where this package builds (hosts/misfit/default.nix
  # sets buildOnTarget).
  sourceRoot = ".";

  # stdenv's strip hook rewrites the ELF the same way patchelf does, so it gets
  # the same veto: the appended Bun payload has to survive byte for byte.
  dontPatchELF = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    # 4 MB of upstream's bundling output, never executed.
    rm -f configarr.map

    install -Dm755 configarr $out/libexec/configarr
    # The binary is dynamically linked against /lib64/ld-linux-x86-64.so.2, which
    # NixOS does not have.  printf, not a heredoc: a heredoc here would inherit
    # this file's indentation and break the shebang.
    mkdir -p $out/bin
    printf '#!/bin/sh\n# Names the loader instead of patchelf-ing — patchelf drops the\n# appended Bun payload, see the header of packages/configarr.nix.\nexec %s/lib/ld-linux-x86-64.so.2 %s/libexec/configarr "$@"\n' \
      "${glibc}" "$out" > $out/bin/configarr
    chmod 755 $out/bin/configarr

    runHook postInstall
  '';

  meta = {
    description = "Sync TRaSH-Guides custom formats, quality definitions and profiles to Radarr and Sonarr";
    homepage = "https://github.com/raydak-labs/configarr";
    license = lib.licenses.isc;
    mainProgram = "configarr";
    platforms = [ "x86_64-linux" ];
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
})
