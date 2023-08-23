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
  haskellPackages = prev.haskellPackages.override {
    overrides = haskellPackagesNew: _haskellPackagesOld: rec {
      ws-access-token =
        haskellPackagesNew.callPackage ./ws-access-token/release.nix { };
      resource-id =
        haskellPackagesNew.callPackage ./resource-id/release.nix { };
      mono-stretchly = prev.haskell.lib.compose.overrideCabal
        (drv: {
          postInstall =
            if prev.stdenv.isDarwin then ''
              ${drv.postInstall or ""}
              mkdir -p $out/Applications/Mono-Stretchly.app/Contents/MacOS
              ln -s $out/bin $out/Applications/Mono-Stretchly.app/Contents/MacOS
            '' else ''
              ${drv.postInstall or ""}
            ''
          ;
        })
        (haskellPackagesNew.callPackage ./mono-stretchly/project.nix { });
      hi-chew = haskellPackagesNew.callPackage ./hi-chew/release.nix { };
    };
  };
}
