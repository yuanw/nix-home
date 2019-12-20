self: super: {

installApplication = 
  { name, appname ? name, version, src, description, homepage, 
    postInstall ? "", sourceRoot ? ".", ... }:
  with super; stdenv.mkDerivation {
    name = "${name}-${version}";
    version = "${version}";
    src = src;
    buildInputs = [ undmg unzip ];
    sourceRoot = sourceRoot;
    phases = [ "unpackPhase" "installPhase" ];
    installPhase = ''
      mkdir -p "$out/Applications/${appname}.app"
      cp -pR * "$out/Applications/${appname}.app"
    '' + postInstall;
    meta = with stdenv.lib; {
      description = description;
      homepage = homepage;
      maintainers = maintainers;
      platforms = platforms.darwin;
    };
  };

CopyQ = self.installApplication rec {
  name = "CopyQ";
  version = "3.9.3";
  sourceRoot = "CopyQ.app";
  src = super.fetchurl {
    url = "https://github.com/hluk/CopyQ/releases/download/v${version}/CopyQ.dmg";
    sha256 = "d18188201a2a40ca65f5e289149d0166785a5aa7376b77b6a690b40189c50520";
    # date = 2019-10-23T09:49:21-0700;
  };
  description = ''
    CopyQ is advanced clipboard manager with editing and scripting features.
  '';
  homepage = https://hluk.github.io/CopyQ/;
};
}
