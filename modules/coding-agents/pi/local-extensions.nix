# Home Manager modules that expose local .ts extensions as
# programs.pi.extensions.<name> (nixpi mkPiExtensionModule pattern).
{
  inputs,
  pkgs,
}:
let
  inherit (inputs.nixpi.lib.nixpi) mkPiExtension mkPiExtensionModule;
  extDir = ./extensions;

  mkLocalExtension =
    name: file:
    mkPiExtensionModule {
      inherit name;
      description = "Local Pi ${name} extension";
      package = mkPiExtension {
        inherit pkgs;
        pname = name;
        version = "0.1.0";
        entrypoint = extDir + "/${file}";
      };
    };
in
[
  (mkLocalExtension "notify" "notify.ts")
  (mkLocalExtension "custom-footer" "custom-footer.ts")
  (mkLocalExtension "web-fetch" "web-fetch.ts")
  (mkLocalExtension "direnv" "direnv.ts")
]
