/*
  Convert nix-home pi extension derivations / local files into Pi packages that
  nixpi can put in settings.packages.

  Bare store paths in settings.extensions skip Pi's host peer-dep mapping
  (@mariozechner/*, @sinclair/typebox). Local Pi packages get that mapping.
  Packages that already ship a `pi` manifest are reused; their skills are
  cleared so agent-pm remains the skill installer.
*/
{
  lib,
  pkgs,
  mkPiExtension,
}:
let
  hasPiManifest =
    pkg:
    builtins.pathExists "${pkg}/package.json"
    && ((builtins.fromJSON (builtins.readFile "${pkg}/package.json")) ? pi);

  # Keep extension(s) from an existing Pi package; drop skills/prompts/themes so
  # we do not fight programs.agent-pm or duplicate resources.
  stripNonExtensions =
    pkg:
    let
      pname = pkg.pname or "pi-package";
      version = pkg.version or "0.1.0";
      orig = builtins.fromJSON (builtins.readFile "${pkg}/package.json");
      pi = (orig.pi or { }) // {
        skills = [ ];
        prompts = [ ];
        themes = [ ];
      };
      packageJson = pkgs.writeText "${pname}-package.json" (builtins.toJSON (orig // { inherit pi; }));
    in
    pkgs.runCommand "pi-package-${pname}-${version}"
      {
        inherit pname version;
        passthru = pkg.passthru or { };
      }
      ''
        mkdir -p "$out"
        cp -a ${pkg}/. "$out/"
        chmod -R u+w "$out"
        cp ${packageJson} "$out/package.json"
      '';

  # Multi-file directory without package.json (e.g. permission-gate).
  wrapDirectory =
    pkg:
    let
      pname = lib.removeSuffix ".ts" (pkg.pname or "extension");
      version = pkg.version or "0.1.0";
    in
    mkPiExtension {
      inherit pkgs pname version;
      src = pkgs.runCommand "${pname}-pi-src" { } ''
        mkdir -p "$out/extensions"
        cp -a ${pkg}/. "$out/extensions/"
      '';
    };

  # Single-file derivation ($out is the .ts file) or a path to one.
  wrapEntrypoint =
    {
      pname,
      version ? "0.1.0",
      entrypoint,
    }:
    mkPiExtension {
      inherit pkgs version entrypoint;
      pname = lib.removeSuffix ".ts" pname;
    };

  pathType =
    path:
    if builtins ? readFileType then
      builtins.readFileType path
    else if builtins.pathExists "${path}/index.ts" || builtins.pathExists "${path}/package.json" then
      "directory"
    else
      "regular";
in
{
  # pkg: derivation from packages/pi-extensions
  fromExtensionPkg =
    pkg:
    let
      kind = pathType pkg;
    in
    if hasPiManifest pkg then
      stripNonExtensions pkg
    else if kind == "directory" then
      wrapDirectory pkg
    else
      wrapEntrypoint {
        pname = pkg.pname or "extension";
        version = pkg.version or "0.1.0";
        entrypoint = pkg;
      };

  # name: filename (notify.ts); path: source file
  fromExtensionFile =
    name: path:
    wrapEntrypoint {
      pname = name;
      entrypoint = path;
    };
}
