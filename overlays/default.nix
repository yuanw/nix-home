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

  # mainstream use ${nix}, somehow it is not pointing to nix2.4
  nix-direnv = prev.nix-direnv.overrideAttrs (old: rec {
    postPatch = ''
      sed -i "1a NIX_BIN_PREFIX=${final.nixFlakes}/bin/" direnvrc
      substituteInPlace direnvrc --replace "grep" "${final.gnugrep}/bin/grep"
    '';

  });

  alerter = prev.callPackage ./alerter.nix { };
  dart = prev.callPackage ./dart.nix { };
  hls = prev.callPackage ./easy-hls.nix { };
  hosts = prev.callPackage ./hosts.nix { };

  pragmata-pro = prev.callPackage ./pragmata-pro-font.nix { };

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
