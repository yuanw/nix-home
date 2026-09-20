{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  nix-update-script,
  ...
}:
let
  rev = "df4771e9105d29bde9b8f32858df6139c1c90605";
in
buildNpmPackage (finalAttrs: {
  pname = "pi-interactive-shell";
  version = "0.13.0-unstable-2026-04-24";

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-interactive-shell";
    inherit rev;
    hash = "sha256-80E/Mw9id7b2KoJ2iLAOb2hXILIdqNZ00PxlvPQ+08g=";
  };

  postPatch = ''
    cp ${./pi-interactive-shell.package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-jGXdGx5vEkpg5ECyFhuxepevwOPaXSLN1HILubq0/YI=";

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r *.ts *.json node_modules $out/
    runHook postInstall
  '';

  passthru = {
    piExtension = {
      pname = finalAttrs.pname;
      version = finalAttrs.version;
    };
    updateScript = nix-update-script { };
  };

  meta = {
    description = "Run AI coding agents in pi TUI overlays with interactive shell";
    homepage = "https://github.com/nicobailon/pi-interactive-shell";
    license = lib.licenses.mit;
  };
})
