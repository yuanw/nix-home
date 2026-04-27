{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
  ...
}:

buildNpmPackage (_finalAttrs: {
  pname = "pi-mcp-adapter";
  version = "2.5.1";

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-mcp-adapter";
    rev = "720b677d36320e3c2ed1376a89239a66868c8ac9";
    hash = "sha256-Vr1wD6ABj9ZNBgBEpxtvRHYbvUTukeqrS05btiQ5SBo=";
  };

  npmDepsHash = "sha256-Muc0bUf3N9R/1qrD9pn8/1WgNf2UqJWkcQ1nJmcjoeE=";

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r package.json node_modules app-bridge.bundle.js $out/
    for f in *.ts; do
      case "$f" in
        *.test.ts|vitest.config.ts) ;;
        *) cp "$f" "$out/" ;;
      esac
    done
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "MCP (Model Context Protocol) adapter extension for Pi coding agent";
    homepage = "https://github.com/nicobailon/pi-mcp-adapter";
    license = lib.licenses.mit;
  };
})
