{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
  nix-update-script,
  ...
}:

buildNpmPackage (finalAttrs: {
  pname = "pi-cursor-agent";
  version = "0.4.4";

  src = fetchFromGitHub {
    owner = "sudosubin";
    repo = "pi-frontier";
    rev = "${finalAttrs.pname}@${finalAttrs.version}";
    hash = "sha256-31ixjrEOfU7S4BcGjdVjp1RTr8xyZqbGpjC2Ec0njIQ=";
  };

  sourceRoot = "${finalAttrs.src.name}/pi-cursor-agent";

  npmDepsHash = "sha256-aePcIq6OatykdUVBs3ECc5USw5Fd/1HrLHJ/IUfJACU=";

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r package.json src node_modules $out/
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--version-regex=pi-cursor-agent@(.+)"
    ];
  };

  meta = {
    description = "Cursor Agent provider extension for pi";
    homepage = "https://github.com/sudosubin/pi-frontier/tree/main/pi-cursor-agent";
    license = lib.licenses.mit;

  };
})
