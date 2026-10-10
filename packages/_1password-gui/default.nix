prev:
# 1Password desktop app, pinned by ./sources.json instead of by nixpkgs.
#
# Why this exists: nixpkgs keeps the version and the per-platform hashes of
# pkgs._1password-gui in pkgs/by-name/_1/_1password-gui/sources.json, which that
# package's own update.sh refreshes against 1Password's Linux APT index and its
# Mac app-update feed.  That is the whole design until the app breaks: the
# nix-darwin module rsyncs the bundle into /Applications read-only, so a
# 1Password that trails the release has an update it can check for, download,
# and never install.  Waiting for nixpkgs to notice is not an option then, so
# this file takes the newest release directly, on the same nixpkgs, with
#   just bump-1password          (scripts/bump-1password)
#
# overrideAttrs, not override: package.nix binds `version` and `src` in a let,
# so they are not arguments of the function `override` re-applies -- passing them
# through `override` is silently ignored, and the pin would do nothing at all.
#
# A pin only applies when it is NEWER than nixpkgs.  Once nixpkgs catches up the
# override stops firing, so a host that has not run bump-1password in months
# cannot be quietly downgraded by a nixpkgs bump.
#
# A platform with no entry here keeps nixpkgs' package.  Only darwin/aarch64
# (hosts/mist.nix) and linux/x86_64 (hosts/asche/configuration.nix) enable
# 1Password today; add to sources.json when that changes.
let
  pins = builtins.fromJSON (builtins.readFile ./sources.json);
  kernel = prev.stdenv.hostPlatform.parsed.kernel.name; # "darwin" | "linux"
  cpu = prev.stdenv.hostPlatform.parsed.cpu.name; # "aarch64" | "x86_64"
  pin = pins.${kernel} or { };
  source = (pin.sources or { }).${cpu} or null;
in
if !prev.lib.versionOlder prev._1password-gui.version (pin.version or "0") || source == null then
  prev._1password-gui
else
  prev._1password-gui.overrideAttrs (
    _finalAttrs: _previousAttrs: {
      version = pin.version;
      src = prev.fetchurl {
        inherit (source) url hash;
      };
    }
  )
