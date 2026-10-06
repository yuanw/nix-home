# Home Manager modules for pkgs.pi-extensions via nixpi mkPiExtensionModule.
{
  inputs,
  pkgs,
  lib,
}:
let
  inherit (inputs.nixpi.lib.nixpi) mkPiExtensionModule;
  toNixpiPackage = import ./to-nixpi-package.nix {
    inherit lib pkgs;
    mkPiExtension = inputs.nixpi.lib.nixpi.mkPiExtension;
  };

  mk =
    name: pkg:
    mkPiExtensionModule {
      inherit name;
      description = "Pi ${name} extension";
      package = toNixpiPackage.fromExtensionPkg pkg;
    };
in
[
  (mk "cursor-agent" pkgs.pi-extensions.pi-cursor-agent)
  (mk "slow-mode" pkgs.pi-extensions.pi-slow-mode)
  (mk "permission-gate" pkgs.pi-extensions.pi-permission-gate)
  (mk "interactive-shell" pkgs.pi-extensions.pi-interactive-shell)
  (mk "ponytail" pkgs.pi-extensions.pi-ponytail)
]
