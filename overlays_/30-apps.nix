self: super: {

  installApplication =
    { name
    , appname ? name
    , version
    , src
    , description
    , homepage
    , postInstall ? ""
    , sourceRoot ? "."
    , ...
    }:
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

  HandBrake = self.installApplication rec {
    name = "HandBrake";
    version = "1.3.1";
    sourceRoot = "HandBrake.app";
    src = super.fetchurl {
      url = "https://github.com/HandBrake/HandBrake/releases/download/${version}/HandBrake-${version}.dmg";
      sha256 = "0vm2f7sbb2i5icsmc06q698myclp455cj855kw93cj2s33bfn4xp";
    };
    description = ''
      HandBrake is a tool for converting video from nearly any format to a
      selection of modern, widely supported codecs
    '';
    homepage = https://handbrake.fr;
  };


  Docker = self.installApplication rec {
    name = "Docker";
    version = "2.1.0.5";
    sourceRoot = "Docker.app";
    src = super.fetchurl {
      url = https://download.docker.com/mac/stable/Docker.dmg;
      sha256 = "0b80j320wnidc13w38b1m68r2adf7skgxnmhk8zbxmrksn7nrrgh";
      # date = 2019-10-23T09:49:21-0700;
    };
    description = ''
      Docker CE for Mac is an easy-to-install desktop app for building,
      debugging, and testing Dockerized apps on a Mac
    '';
    homepage = https://store.docker.com/editions/community/docker-ce-desktop-mac;
  };
}
