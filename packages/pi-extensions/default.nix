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
  localExt = ../../modules/coding-agents/pi/extensions;
in
{
  inherit mkPiExtension mkLocalPiExtension;
  pi-interactive-shell = callExtension ./pi-interactive-shell.nix;
  pi-cursor-agent = callExtension ./pi-cursor-agent;
  pi-slow-mode = callExtension ./pi-slow-mode.nix;
  pi-permission-gate = callExtension ./pi-permission-gate.nix;
  pi-ponytail = callExtension ./pi-ponytail.nix;
  # Local single-file extensions (modules/coding-agents/pi/extensions)
  pi-notify = mkLocalPiExtension "notify.ts" (localExt + "/notify.ts");
  pi-custom-footer = mkLocalPiExtension "custom-footer.ts" (localExt + "/custom-footer.ts");
  pi-web-fetch = mkLocalPiExtension "web-fetch.ts" (localExt + "/web-fetch.ts");
  pi-direnv = mkLocalPiExtension "direnv.ts" (localExt + "/direnv.ts");
}
