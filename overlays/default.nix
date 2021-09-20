final: prev:

let
  tatContent = builtins.readFile ./tat;
  taContent = builtins.readFile ./ta;

in {
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
    # stolen from https://github.com/cmacrae/config/tree/master/patches
    patches = [ ./patches/borderless-emacs.patch ];
  });

  # gap marked as broken for darwin, it seems build on my mac
  gap = prev.gap.overrideAttrs (oldAttrs: rec { meta.broken = false; });

  emacsCatalina = prev.emacs.overrideAttrs (o: rec {

    patches = [ ./patches/fix-window-role-yabai.patch ];

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

  alerter = prev.callPackage ./alerter.nix { };
  dart = prev.callPackage ./dart.nix { };
  hls = prev.callPackage ./easy-hls.nix { };
  hosts = prev.callPackage ./hosts.nix { };

  pragmata-pro = prev.callPackage ./pragmata-pro-font.nix { };

  cpu-stats = final.pkgs.writeShellScriptBin "cpuStat" ''
    ps -A -o %cpu | awk '{s+=$1} END {print s "%"}'
  '';

  tat = final.pkgs.writeShellScriptBin "tat" tatContent;
  ta = final.pkgs.writeShellScriptBin "ta" taContent;

  juliaMac = final.installApplication rec {
    name = "Julia";
    version = "1.6.2";
    sourceRoot = "Julia-1.6.app";
    src = prev.fetchurl {
      url =
        "https://julialang-s3.julialang.org/bin/mac/x64/1.6/julia-1.6.2-mac64.dmg";
      sha256 = "1j9pqi7lvh8v1j81bpy7gjaa7kdr4s92qkc27rdp2z6wl48f3dbg";
    };
    description = "High Performance";
    homepage = "https://julialang.org/";
    postInstall = ''
      mkdir -p $out/bin
      for file in $out/Applications/Julia.app/Contents/Resources/julia/bin/julia
      do
        ln -s $file $out/bin/julia
        chmod +x $file
      done
    '';
  };

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
}
