# nix-prefetch-github nicobailon pi-better --rev v<version>
{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
  ...
}:
let
  version = "0.2.0";
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "pi-better";
  inherit version;

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-better";
    rev = "v${version}";
    hash = "sha256-mhWARTnQY2MzrXw3NEmU1r2dQF3jNNWksMC/jkA+KLE=";
  };

  dontBuild = true;

  # The runtime files from package.json "files". The @earendil-works/pi-*
  # peers come from Pi at load time and the tests are not shipped, so there is
  # nothing to install and no node_modules to build.
  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp package.json index.ts better.ts helper.md brief.md $out/
    runHook postInstall
  '';

  passthru.piExtension = {
    pname = finalAttrs.pname;
    inherit (finalAttrs) version;
  };

  meta = {
    description = ''
      /better rewrites your prompt with Felix Rieseberg's "Depth" prompt, using
      your real code and files, and puts it in the input box
    '';
    homepage = "https://github.com/nicobailon/pi-better";
    license = lib.licenses.mit;
  };
})
