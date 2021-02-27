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
      meta = with stdenv.lib; {
        inherit description;
        inherit homepage;
        inherit maintainers;
        platforms = platforms.darwin;
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
