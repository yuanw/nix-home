_final: prev: {
  installApplication =
    { name
    , version
    , src
    , description
    , homepage
    , sourceRoot ? "."
    , ...
    }:
      with _final; stdenv.mkDerivation {
        name = "${name}-${version}";
        version = "${version}";
        src = src;
        buildInputs = [ prev.pkgs._7zz ];
        sourceRoot = ".";
        phases = [ "unpackPhase" "installPhase" ];
        unpackCmd = ''
          7zz x $src -snld
        '';
        installPhase = ''
          runHook preInstall
          mkdir -p $out/Applications
          cp -r calibre*.app "$out/Applications/"

          mkdir -p $out/bin

          ln -s "$out/Applications/calibre.app/Contents/MacOS/ebook-convert" "$out/bin/ebook-convert"


          runHook postInstall
        '';

        meta = with super.lib; {
          description = description;
          homepage = homepage;
          platforms = platforms.darwin;
        };
      };
  # https://github.com/Homebrew/homebrew-cask/blob/f144ade7bcc8884fdf2a57b114cf11e7d98b2c93/Casks/c/calibre.rb
  calibre_mac = _final.installApplication rec {
    name = "calibre";
    version = "7.9.0";
    src = prev.fetchurl {
      url = "https://download.calibre-ebook.com/${version}/calibre-${version}.dmg";
      # hash = prev.lib.fakeHash;
      hash = "sha256-1gdTsAdYUKBgltlarblZ/6fPwe+CGGVeFT0b9LOatw0=";

    };
    description = "e-book library management";
    homepage = "https://calibre-ebook.com/";
  };

  alerter = prev.callPackage ./alerter { };
  dart = prev.callPackage ./dart.nix { };
  hosts = prev.callPackage ./hosts.nix { };
  reveal-js = prev.callPackage ./reveal-js.nix { };
  # https://github.com/NixOS/nixpkgs/pull/295747
  # yabai = prev.yabai.overrideAttrs (_finalAttrs: _previousAttrs: {
  #   version = "7.0.2";
  #   src =
  #     if prev.hostPlatform.system == "x86_64-darwin" then
  #       (
  #         prev.fetchFromGitHub {
  #           owner = "koekeishiya";
  #           repo = "yabai";
  #           rev = "v7.0.2";
  #           hash = "sha256-/MOAKsY7MlRWdvUQwHeITTeGJbCUdX7blZZAl2zXuic=";
  #         }
  #       ) else
  #       (prev.fetchzip {
  #         url = "https://github.com/koekeishiya/yabai/releases/download/v7.0.2/yabai-v7.0.2.tar.gz";
  #         hash = "sha256-FeNiJJM5vdzFT9s7N9cTjLYxKEfzZnKE9br13lkQhJo=";
  #       });
  # });

  choose-mac = prev.callPackage ./choose-mac.nix { };

  jdt-language-server = prev.jdt-language-server.overrideAttrs (_finalAttrs: _previousAttrs: {
    version = "1.36.0";
    src = prev.fetchurl {
      url = "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.36.0/jdt-language-server-1.36.0-202405301306.tar.gz";
      # hash = prev.lib.fakeHash;
      hash = "sha256-Ao4nTQb0phytT/1W+J70FKj2VhPG0F2UZ2Ubf7A9rns=";
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
