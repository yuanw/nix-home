# https://github.com/NixOS/nixpkgs/pull/279285/files
{ lib
, stdenv
, fetchurl
, python3
}:

let
  timestamp = "202402151717";
in
stdenv.mkDerivation (finalAttrs: {
  pname = "jdt-language-server";
  version = "1.33.0";

  src = fetchurl {
    url = "https://download.eclipse.org/jdtls/milestones/${finalAttrs.version}/jdt-language-server-${finalAttrs.version}-${timestamp}.tar.gz";
    hash = "sha256-UZQQl3lFPmN6Azglf97xevwA6OehO/2bSM0bg93z8YY=";
  };

  sourceRoot = ".";

  buildInputs = [
    # Used for the included wrapper
    python3
  ];

  postPatch = ''
    # We store the plugins, config, and features folder in different locations
    # than in the original package.
    substituteInPlace bin/jdtls.py \
      --replace "
      jdtls_base_path = Path(__file__).parent.parent" "jdtls_base_path = Path(\"$out/share/java/jdtls/\")"
  '';

  installPhase =
    let
      # The application ships with different config directories for each platform.
      # Note the application come with ARM variants as well, although the
      # current included wrapper doesn't use them.
      configDir = if stdenv.isDarwin then "config_mac" else "config_linux";
    in
    ''
      install -Dm444 -t $out/share/java/jdtls/plugins/ plugins/*
        install -Dm444 -t $out/share/java/jdtls/features/ features/*
        install -Dm444 -t $out/share/java/jdtls/${configDir} ${configDir}/*
        install -Dm555 -t $out/bin bin/jdtls
        install -Dm444 -t $out/bin bin/jdtls.py
    '';

  meta = {
    homepage = "https://github.com/eclipse/eclipse.jdt.ls";
    description = "Java language server";
    sourceProvenance = with lib.sourceTypes; [ binaryBytecode ];
    license = lib.licenses.epl20;
    maintainers = with lib.maintainers; [ matt-snider ];
    platforms = lib.platforms.all;
    mainProgram = "jdtls";
  };
})

