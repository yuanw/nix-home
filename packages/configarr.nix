# Configarr — the TRaSH-Guides syncer for Radarr/Sonarr (custom formats, quality
# definitions, quality profiles).  Planned in
# nix-home-private/docs/misfit-arr.org [[#configarr]]; it is *not* in nixpkgs, so
# this file is the whole reason `pkgs.configarr` exists.
#
# Upstream ships no source-buildable artifact for us to compile: the repo is a
# pnpm/TypeScript app whose only distribution is a Docker image and per-platform
# tarballs.  The release asset is a single self-contained Node executable, so the
# packaging here is a fetch plus a patchelf — `file` on the downloaded asset says
# "ELF 64-bit LSB executable, x86-64, dynamically linked (interpreter
# /lib64/ld-linux-x86-64.so.2), GLIBC_2.2.5+", which is exactly what
# autoPatchelfHook is for.
#
# Two things the unit in hosts/misfit/configuration.nix has to carry because the
# binary cannot: `git` on PATH (it clones TRaSH-Guides and the recyclarr template
# repo with simple-git, i.e. it shells out — the strings carry "Failed to spawn
# git process"), and a writable working directory (src/env.ts puts its cloned
# repos at $ROOT_PATH/repos, ROOT_PATH defaulting to the cwd).  It takes no CLI
# flags at all: every knob is an environment variable (CONFIG_LOCATION,
# SECRETS_LOCATION, DRY_RUN, LOG_LEVEL — read off src/env.ts at v1.39.2).
{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "configarr";
  version = "1.39.2";

  src = fetchurl {
    url = "https://github.com/raydak-labs/configarr/releases/download/v${finalAttrs.version}/configarr-linux-x64.tar.xz";
    hash = "sha256-4BzDvgZ9HkOMG0sTx9l52gJUONdHr7yL9Aic4FZmjvE=";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  # The tarball holds one executable and its source map; the map is a debugging
  # artifact of upstream's bundling step and 4 MB of closure we do not run.
  installPhase = ''
    runHook preInstall
    install -Dm755 configarr $out/bin/configarr
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
