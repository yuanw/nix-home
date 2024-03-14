_final: prev: {
  alerter = prev.callPackage ./alerter { };
  dart = prev.callPackage ./dart.nix { };
  hosts = prev.callPackage ./hosts.nix { };
  yabai = prev.yabai.overrideAttrs (_finalAttrs: _previousAttrs: {
    version = "7.0.2";
    src =
      prev.fetchzip {
        url = "https://github.com/koekeishiya/yabai/releases/download/v7.0.2/yabai-v7.0.2.tar.gz";
        hash = "sha256-FeNiJJM5vdzFT9s7N9cTjLYxKEfzZnKE9br13lkQhJo=";
      };
  });


  jdt-language-server = prev.jdt-language-server.overrideAttrs (_finalAttrs: _previousAttrs: {
    version = "1.33.0";
    src = prev.fetchurl {
      url = "https://download.eclipse.org/jdtls/milestones/1.33.0/jdt-language-server-1.33.0-202402151717.tar.gz";
      hash = "sha256-UZQQl3lFPmN6Azglf97xevwA6OehO/2bSM0bg93z8YY=";
    };
  });
  sketchybar-app-font = prev.callPackage ./sketchybar-app-font.nix { };
  sf-symbols = prev.callPackage ./sf_symbols.nix { };
  font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
  ical-buddy = prev.callPackage ./ical-buddy.nix { };
  sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };
}
