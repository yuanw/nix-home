let sources = import ../nix/sources.nix;
in
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

      with super;
      stdenv.mkDerivation {
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
      url =
        "https://github.com/hluk/CopyQ/releases/download/v${version}/CopyQ.dmg";
      sha256 = "13c4y43cbwxnlc04xpr7x3fn27cfcjn3mwnyvnr7bnaabhqbczb4";
      # date = 2019-10-23T09:49:21-0700;
    };
    description = ''
      CopyQ is advanced clipboard manager with editing and scripting features.
    '';
    homepage = "https://hluk.github.io/CopyQ/";
  };

  Docker = self.installApplication rec {
    name = "Docker";
    version = "2.1.0.5";
    sourceRoot = "Docker.app";
    src = super.fetchurl {
      url = "https://download.docker.com/mac/stable/Docker.dmg";
      sha256 = "14dgvicl56lzr0p0g1ha7zkqv7wk3kxl90a6zk2cswyxn93br04s";
      # date = 2019-10-23T09:49:21-0700;
    };
    description = ''
      Docker CE for Mac is an easy-to-install desktop app for building,
      debugging, and testing Dockerized apps on a Mac
    '';
    homepage =
      "https://store.docker.com/editions/community/docker-ce-desktop-mac";
  };

  Stretchly = self.installApplication rec {
    name = "Stretchly";
    version = "1.2.0";
    sourceRoot = "Stretchly.app";
    src = super.fetchurl { inherit (sources.Stretchly) url sha256; };
    description = "break time reminder app";
    homepage = "https://hovancik.net/stretchly/";
  };
}
