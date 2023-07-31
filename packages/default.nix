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
}
