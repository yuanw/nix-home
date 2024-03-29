_final: prev: {
  alerter = prev.callPackage ./alerter { };
  dart = prev.callPackage ./dart.nix { };
  hosts = prev.callPackage ./hosts.nix { };
  # https://github.com/NixOS/nixpkgs/pull/295747
  yabai = prev.yabai.overrideAttrs (_finalAttrs: _previousAttrs: {
    version = "7.0.2";
    src =
      if prev.hostPlatform.system == "x86_64-darwin" then
        (
          prev.fetchFromGitHub {
            owner = "koekeishiya";
            repo = "yabai";
            rev = "v7.0.2";
            hash = "sha256-/MOAKsY7MlRWdvUQwHeITTeGJbCUdX7blZZAl2zXuic=";
          }
        ) else
        (prev.fetchzip {
          url = "https://github.com/koekeishiya/yabai/releases/download/v7.0.2/yabai-v7.0.2.tar.gz";
          hash = "sha256-FeNiJJM5vdzFT9s7N9cTjLYxKEfzZnKE9br13lkQhJo=";
        });
  });


  jdt-language-server = prev.jdt-language-server.overrideAttrs (_finalAttrs: _previousAttrs: {
    version = "1.33.0";
    src = prev.fetchurl {
      url = "https://download.eclipse.org/jdtls/milestones/1.33.0/jdt-language-server-1.33.0-202402151717.tar.gz";
      hash = "sha256-UZQQl3lFPmN6Azglf97xevwA6OehO/2bSM0bg93z8YY=";
    };
  });
  sf-symbols = prev.callPackage ./sf_symbols.nix { };
  font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
  ical-buddy = prev.callPackage ./ical-buddy.nix { };
  sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };
  janky-borders = prev.callPackage ./JankyBorders.nix {
    inherit (_final.darwin.apple_sdk_11_0.frameworks) AppKit CoreVideo Carbon SkyLight;
  };
}
