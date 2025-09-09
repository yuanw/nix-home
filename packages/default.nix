_final: prev: {
  installApplication =
    {
      name,
      version,
      src,
      description,
      homepage,
      sourceRoot ? ".",
      ...
    }:
    with _final;
    stdenv.mkDerivation {
      name = "${name}-${version}";
      version = "${version}";
      src = src;
      buildInputs = [ prev.pkgs._7zz ];
      sourceRoot = ".";
      phases = [
        "unpackPhase"
        "installPhase"
      ];
      unpackCmd = ''
        7zz x $src -snld
      '';
      installPhase = ''
        runHook preInstall
        mkdir -p $out/Applications
        cp -r *.app "$out/Applications/"

        mkdir -p $out/bin

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
    version = "8.4.0";
    src = prev.fetchurl {
      url = "https://download.calibre-ebook.com/${version}/calibre-${version}.dmg";
      # hash = prev.lib.fakeHash;
      hash = "sha256-NngNwMrQ0bLhDYc/Gnf/cFisAC+fMR7JQeXBjH/+m68=";
    };
    description = "e-book library management";
    homepage = "https://calibre-ebook.com/";
    # postInsall ln -s "$out/Applications/calibre.app/Contents/MacOS/ebook-convert" "$out/bin/ebook-convert"
  };
  ghostty-mac = _final.installApplication rec {
    name = "ghostty";
    version = "1.0.0";
    src = prev.fetchurl {
      url = "https://release.files.ghostty.org/1.0.0/Ghostty.dmg";
      #hash = prev.lib.fakeHash;
      hash = "sha256-CR96Kz9BYKFtfVKygiEku51XFJk4FfYqfXACeYQ3JlI=";
    };
    homepage = "a";
    description = "ghostty";
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
  # jdt-language-server = prev.jdt-language-server.overrideAttrs (
  #   _finalAttrs: _previousAttrs: {
  #     postPatch = ''
  #       # We store the plugins, config, and features folder in different locations
  #       # than in the original package. In addition, hard-code the path to the jdk
  #       # in the wrapper, instead of searching for it in PATH at runtime.
  #       substituteInPlace bin/jdtls.py \
  #         --replace "jdtls_base_path = Path(__file__).parent.parent" "jdtls_base_path = Path(\"$out/share/java/jdtls/\")"
  #     '';
  #   }
  # );

  haskellPackages = prev.haskellPackages.override {
    overrides = haskellPackagesNew: _haskellPackagesOld: rec {
      ask = haskellPackagesNew.callPackage ./ask/release.nix { };
    };
  };
  bandcamp-dl = prev.python3Packages.callPackage ./bandcamp { };
  choose-mac = prev.callPackage ./choose-mac.nix { };
  sf-symbols = prev.callPackage ./sf_symbols.nix { };
  font-hack-nerd-font = prev.callPackage ./font-hack-nerd-font.nix { };
  # ical-buddy = prev.callPackage ./ical-buddy.nix { };
  sketchybar-cpu-helper = prev.callPackage ./sketchybar-cpu-helper { };
  aws-iam-authenticator = prev.callPackage ./aws-iam-authenticator.nix { };

  mermaid-cli-wrapped = prev.callPackage ./mermaid-cli-wrapped.nix {

  };
}
