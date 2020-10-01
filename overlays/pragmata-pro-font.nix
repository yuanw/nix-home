self: super: {
  pragmata-pro-font = with super; stdenv.mkDerivation rec {
    nativeBuildInputs = [
      unzip
    ];
    pname ="pragmata-pro-font";
    version = "1.2";
    buildInputs = [ unzip ];

    src = ../private/EssentialPragmataPro-Regular1.2.zip;
    sha256 = "123";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/share/fonts/Essential-PragmataPro
      unzip $src
      cp EssentialPragmataPro-Regular1.2/*.ttf $out/share/fonts/Essential-PragmataPro
    '';
  };
}
