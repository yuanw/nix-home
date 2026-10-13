{ pkgs, ... }:
let
  # Build a single pi extension (.ts file) derivation.
  # pname must include the .ts suffix when the package is a single file.
  mkPiExtension =
    {
      pname,
      version,
      src,
      # Path of the .ts file within src
      tsPath,
    }:
    pkgs.stdenvNoCC.mkDerivation {
      inherit pname version src;
      dontBuild = true;

      installPhase = ''
        runHook preInstall
        install -Dm644 ${tsPath} $out
        runHook postInstall
      '';

      passthru.piExtension = {
        inherit pname version;
      };
    };

  # Package a local .ts file as a pi extension.
  # pname must include the .ts suffix (used as the filename in extensions/).
  # Example: mkLocalPiExtension "notify.ts" ./extensions/notify.ts
  mkLocalPiExtension =
    pname: src:
    pkgs.runCommand pname
      {
        inherit pname;
        passthru.piExtension = { inherit pname; };
      }
      ''
        install -Dm644 ${src} $out
      '';

  callExtension = path: pkgs.callPackage path { inherit mkPiExtension; };

  # These already ship a working `pi` manifest, so pi-flake lists them directly
  # instead of wrapping them into a second copy (pi-cursor-agent alone is
  # ~400MB of node_modules). Single-file extensions stay unmarked: they have to
  # be wrapped before Pi will map their peer dependencies.
  # extendDerivation annotates without touching the derivation itself.
  asPiPackage = pkg: pkgs.lib.extendDerivation true { isPiPackage = true; } pkg;
in
{
  inherit mkPiExtension mkLocalPiExtension;
  pi-interactive-shell = asPiPackage (callExtension ./pi-interactive-shell.nix);
  pi-cursor-agent = asPiPackage (callExtension ./pi-cursor-agent);
  pi-slow-mode = callExtension ./pi-slow-mode.nix;
  pi-permission-gate = callExtension ./pi-permission-gate.nix;
  pi-ponytail = asPiPackage (callExtension ./pi-ponytail.nix);
}
