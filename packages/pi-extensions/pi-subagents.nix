# nix-prefetch-github nicobailon pi-subagents --rev v<version>
{
  buildNpmPackage,
  fetchFromGitHub,
  lib,
  nix-update-script,
  ...
}:
let
  version = "0.90.0";
in
buildNpmPackage (finalAttrs: {
  pname = "pi-subagents";
  inherit version;

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-subagents";
    rev = "v${version}";
    hash = "sha256-I8vK89gZ2o6nTL3N0vvE2/BHJVGJMS4m/VspBlsGYvE=";
  };

  # Production deps only: acorn, jiti, undici, yaml, @js-temporal/polyfill.
  # The @earendil-works/pi-* peers are supplied by Pi at load time, so the
  # devDependency tree (which pins pi-coding-agent) is never installed.
  npmInstallFlags = [ "--omit=dev" ];

  npmDepsHash = "sha256-qtYTJJ+BN715N9tzLOOPyAM5wymO0jfbVPi0HtRNkZo=";

  dontNpmBuild = true;

  # Runtime files from package.json "files"; Pi reads the `pi` manifest to find
  # index.ts, skills/ and prompts/.
  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r package.json index.ts src agents skills prompts docs \
      README.md CHANGELOG.md node_modules $out/
    cp *.mjs *.cjs $out/
    runHook postInstall
  '';

  passthru = {
    piExtension = {
      pname = finalAttrs.pname;
      inherit (finalAttrs) version;
    };
    updateScript = nix-update-script {
      extraArgs = [
        "--version-regex=v(.*)"
      ];
    };
  };

  meta = {
    description = ''
      Pi extension for async subagent delegation with truncation, artifacts,
      and session sharing
    '';
    homepage = "https://github.com/nicobailon/pi-subagents";
    license = lib.licenses.mit;
  };
})
