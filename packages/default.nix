_final: prev: {
  yabai = prev.callPackage ./yabai.nix { };
  alerter = prev.callPackage ./alerter { };
  dart = prev.callPackage ./dart.nix { };
  hosts = prev.callPackage ./hosts.nix { };
  emacsPlusNativeComp = prev.callPackage ./emacs-plus.nix { };
  sketchybar-app-font = prev.callPackage ./sketchybar-app-font.nix { };
  sf-symbols = prev.callPackage ./sf_symbols.nix { };
  font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
  ical-buddy = prev.callPackage ./ical-buddy.nix { };
  sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };
  ollama = prev.callPackage ./ollama.nix { };
  haskellPackages = prev.haskellPackages.override {
    overrides = haskellPackagesNew: _haskellPackagesOld: rec {
      ws-access-token =
        haskellPackagesNew.callPackage ./ws-access-token/release.nix { };
      resource-id =
        haskellPackagesNew.callPackage ./resource-id/release.nix { };
      mono-stretchly = prev.haskell.lib.compose.overrideCabal
        (_drv: {
          # https://github.com/alacritty/alacritty/tree/master/extra/osx/Alacritty.app
          # installPhase = ''
          #   mkdir -p $out/Applications

          # '';
          passthru = {
            binaryPath = "Applications/mono-stretchly.app/Contents/MacOS/mono-stretchly";
          };
        })
        (haskellPackagesNew.callPackage ./mono-stretchly/project.nix { });
      hi-chew = haskellPackagesNew.callPackage ./hi-chew/release.nix { };
    };
  };
}
