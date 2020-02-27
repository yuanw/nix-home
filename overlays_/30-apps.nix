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
  version = "3.10.0";
  sourceRoot = "CopyQ.app";
  src = super.fetchurl {
    url = "https://github.com/hluk/CopyQ/releases/download/v${version}/CopyQ.dmg";
    sha256 = "13c4y43cbwxnlc04xpr7x3fn27cfcjn3mwnyvnr7bnaabhqbczb4";
    #sha256 = "d18188201a2a40ca65f5e289149d0166785a5aa7376b77b6a690b40189c50520";
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
    # date = 2020-01-15T16:31:05-0800;
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
    sha256 = "1nhvxi4j6sqmln5yv1d97p2ljnii2ip0k49pmhikabb75bkrhw4g";
    # date = 2019-10-23T09:49:21-0700;
  };
  description = ''
    Docker CE for Mac is an easy-to-install desktop app for building,
    debugging, and testing Dockerized apps on a Mac
  '';
  homepage = https://store.docker.com/editions/community/docker-ce-desktop-mac;
};

stretchly = self.installApplication rec {
  name = "stretchly";
  version = "0.21.1";
  sourceRoot = "stretchly.app";
  src = super.fetchurl {
    url = "https://github.com/hovancik/stretchly/releases/download/v${version}/stretchly-${version}-mac.zip";
    sha256 = "b3c36fcabbc33b13f9aca772c674301a3f13b1263318399bbb298565170030dd";
    # date = 2020-01-16T09:49:21-0700;
  };
  description = "break time reminder app";
  homepage = https://hovancik.net/stretchly/;
};

VLC = self.installApplication rec {
  name = "VLC";
  version = "3.0.8";
  sourceRoot = "VLC.app";
  src = super.fetchurl {
    url = "https://get.videolan.org/vlc/3.0.8/macosx/vlc-${version}.dmg";
    sha256 = "0s6075fhnbksigfcwlglx25fm1yj2sdgmy554ygw9k8jrlvsyaj2";
  };
  description = "free and open sources cross-platform multimedia player";
  homepage = https://www.videolan.org/vlc/;
};
}
