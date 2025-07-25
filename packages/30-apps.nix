_self: super: {

  installApplication =
    {
      name,
      appname ? name,
      version,
      src,
      description,
      homepage,
      postInstall ? "",
      sourceRoot ? ".",
      ...
    }:

    with super;
    stdenv.mkDerivation {
      name = "${name}-${version}";
      version = "${version}";
      inherit src;
      buildInputs = [
        undmg
        unzip
      ];
      inherit sourceRoot;
      phases = [
        "unpackPhase"
        "installPhase"
      ];
      installPhase = ''
        mkdir -p "$out/Applications/${appname}.app"
        cp -pR * "$out/Applications/${appname}.app"
      ''
      + postInstall;
      meta = with stdenv.lib; {
        inherit description;
        inherit homepage;
        maintainers = [ ];
        platforms = platforms.darwin;
      };
    };
}
