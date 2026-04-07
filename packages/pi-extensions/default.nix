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

  callExtension = path: pkgs.callPackage path { inherit mkPiExtension; };
in
{
  inherit mkPiExtension;
  pi-loop = callExtension ./pi-loop.nix;
  pi-review = callExtension ./pi-review.nix;
  pi-interactive-shell = callExtension ./pi-interactive-shell.nix;
}
