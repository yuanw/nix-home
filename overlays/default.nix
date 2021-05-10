final: prev:

{
  installApplication = { name, appname ? name, version, src, description
    , homepage, postInstall ? "", sourceRoot ? ".", ... }:

    with prev;
    stdenv.mkDerivation {
      name = "${name}-${version}";
      version = "${version}";
      inherit src;
      buildInputs = [ undmg unzip ];
      inherit sourceRoot;
      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        mkdir -p "$out/Applications/${appname}.app"
        cp -pR * "$out/Applications/${appname}.app"
      '' + postInstall;
      meta = with lib; {
        inherit description;
        inherit homepage;
        inherit maintainers;
        platforms = platforms.darwin;
      };
    };

  emacsMacport = prev.emacsMacport.overrideAttrs (oldAttrs: rec {
    version = "27.2";
    name = "emacs-mac-27.2-8.2";
    macportVersion = "8.2";
    # stolen from https://github.com/cmacrae/config/tree/master/patches
    patches = [ ./patches/borderless-emacs.patch ];
    src = prev.fetchurl {
      url = "mirror://gnu/emacs/emacs-27.2.tar.xz";
      sha256 = "1ff182gjw9wqsbx1kj5gl2r5pbqhp4ar54g04j33fgz6g17cr9xl";
    };
    macportSrc = prev.fetchurl {
      url = "ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-27.2-mac-8.2.tar.gz";
      sha256 = "1bgm2g3ky7rkj1l27wnmyzqsqxzjng7y9bf72ym37wiyhyi2a9za";
    };
  });

  emacsOsxNativeTile = prev.emacsPgtkGcc.overrideAttrs (oldAttrs: rec {
    patches =
      [ ./patches/no-titlebar.patch ./patches/fix-window-role-yabai.patch ];
  });

  Docker = final.installApplication rec {
    name = "Docker";
    version = "3.2.1";
    sourceRoot = "Docker.app";
    src = final.fetchurl {
      url = "https://download.docker.com/mac/stable/Docker.dmg";
      sha256 = "0sxapv6n1adncdi69haadjdylb60h352ay7yq83xz83fyhwl0kf4";
      # date = 2019-10-23T09:49:21-0700;
    };
    description = ''
      Docker CE for Mac is an easy-to-install desktop app for building,
      debugging, and testing Dockerized apps on a Mac
    '';
    homepage =
      "https://store.docker.com/editions/community/docker-ce-desktop-mac";
  };

  Firefox = final.installApplication rec {
    name = "Firefox";
    version = "88.0.1";

    src = prev.fetchurl {
      name = "Firefox-${version}.dmg";
      url =
        "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-CA/Firefox%20${version}.dmg";
      sha256 = "TEZ0JloM8GQbqN2iPs5cFCfBcGus5NHi+2zOqgWv1l0=";
    };
  };

  alerter = prev.callPackage ./alerter.nix { };
  dart = prev.callPackage ./dart.nix { };
  hls = prev.callPackage ./easy-hls.nix { };
  hosts = prev.callPackage ./hosts.nix { };

  pragmata-pro = prev.callPackage ./pragmata-pro-font.nix { };

  cpu-stats = final.pkgs.writeShellScriptBin "cpuStat" ''
    ps -A -o %cpu | awk '{s+=$1} END {print s "%"}'
  '';

  zoom-us = final.installApplication rec {
    name = "zoom-us";
    version = "5.5.5";
    sourceRoot = "zoom.us.app";
    src = prev.fetchurl {
      url = "https://zoom.us/client/latest/Zoom.pkg";
      sha256 = "18m737c3vphqx2gxk4x14a9lg5ad8gx0awkxmrw0sfv6q5gqzrv2";
    };
  };

  Stretchly = final.installApplication rec {
    name = "Stretchly";
    version = "1.7.0";
    sourceRoot = "Stretchly.app";
    src = prev.fetchurl {
      url =
        "https://github.com/hovancik/stretchly/releases/download/v${version}/Stretchly-${version}.dmg";
      sha256 = "158hc7m4x6y4d0xzslp4ydyg8yq5qr122nrb4s5inkg24qx3l3gw";
    };
    description = "break time reminder app";
    homepage = "https://hovancik.net/stretchly/";
  };

  zellij = prev.callPackage ./zellij.nix {

  };
}
