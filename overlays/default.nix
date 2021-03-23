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
  dart = prev.callPackage ./dart.nix { };
  hls = prev.callPackage ./easy-hls.nix { };
  hosts = prev.callPackage ./hosts.nix { };
  ihp-new = prev.callPackage ./ihp-new.nix { };

  pragmata-pro = prev.callPackage ./pragmata-pro-font.nix { };

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
    version = "1.4.0";
    sourceRoot = "Stretchly.app";
    src = prev.fetchurl {
      url =
        "https://github.com/hovancik/stretchly/releases/download/v1.4.0/Stretchly-1.4.0.dmg";
      sha256 = "0bbk1p4d3vmbsxh9782jmyhfxng0grpzgdf6l9kxspsl0xppjyw3";
    };
    description = "break time reminder app";
    homepage = "https://hovancik.net/stretchly/";
  };
}
