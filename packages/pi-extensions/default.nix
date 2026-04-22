{ pkgs, ... }:
let
  # Build a single pi extension (.ts file) derivation.
  # pname must include the .ts suffix — it is used as the filename in the
  # extensions directory by the pi module (e.g. "pi-loop.ts").
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
  # Example: mkLocalPiExtension "permission-gate.ts" ./extensions/permission-gate.ts
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
in
{
  inherit mkPiExtension mkLocalPiExtension;
  pi-autoresearch = callExtension ./pi-autoresearch.nix;
  pi-loop = callExtension ./pi-loop.nix;
  pi-review = callExtension ./pi-review.nix;
  pi-interactive-shell = callExtension ./pi-interactive-shell.nix;
  pi-cursor-agent = callExtension ./pi-cursor-agent;
  pi-slow-mode = callExtension ./pi-slow-mode.nix;
  pi-tutorial = callExtension ./pi-tutorial.nix;
  earendil-pi-review = callExtension ./earendil-pi-review.nix;
  pi-caveman = callExtension ./pi-caveman.nix;
}
