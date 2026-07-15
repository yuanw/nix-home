{
  lib,
  buildNpmPackage,
  makeWrapper,
  nodejs,
}:

buildNpmPackage rec {
  pname = "spectral";
  version = "6.15.0";

  src = lib.cleanSourceWith {
    src = ./.;
    filter =
      path: _type:
      let
        base = baseNameOf path;
      in
      base != "default.nix";
  };

  npmDepsHash = "sha256-mZtF59SMJmt4SE8o7EmhZhcuvt+f/mEsuNHqtTVJsig=";

  dontNpmBuild = true;
  dontNpmInstall = true;

  npmFlags = [
    "--ignore-scripts"
  ];

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    spectralRoot="$out/lib/spectral"
    mkdir -p "$spectralRoot"
    cp -r node_modules "$spectralRoot/"

    mkdir -p "$out/bin"
    makeWrapper ${lib.getExe nodejs} "$out/bin/spectral" \
      --add-flags "$spectralRoot/node_modules/@stoplight/spectral-cli/dist/index.js" \
      --prefix NODE_PATH : "$spectralRoot/node_modules"

    runHook postInstall
  '';

  doInstallCheck = false;

  meta = with lib; {
    description = "OpenAPI, AsyncAPI, and Arazzo linter";
    homepage = "https://github.com/stoplightio/spectral";
    license = licenses.asl20;
    mainProgram = "spectral";
    platforms = platforms.all;
  };
}
