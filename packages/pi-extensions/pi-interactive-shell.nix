{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  jq,
  nix-update-script,
  ...
}:
buildNpmPackage (finalAttrs: {
  pname = "pi-interactive-shell";
  version = "0.17.0";

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-interactive-shell";
    rev = "v${finalAttrs.version}";
    hash = "sha256-zugoEOJNhjZ7GeGj+Ak6tptmxd2y7/pH5/1jmj1iG2w=";
  };

  # Upstream package-lock.json includes peerDependencies (@earendil-works/pi-*)
  # whose nested packages lack integrity hashes, which breaks fetch-npm-deps.
  # Regenerate with (use registry.npmjs.org, not a corporate mirror):
  #   jq 'del(.peerDependencies, .devDependencies, .peerDependenciesMeta)' package.json \
  #     | sponge package.json && rm -f package-lock.json \
  #     && npm install --package-lock-only --ignore-scripts --registry=https://registry.npmjs.org/
  nativeBuildInputs = [ jq ];

  postPatch = ''
    cp ${./pi-interactive-shell.package-lock.json} package-lock.json
    jq 'del(.peerDependencies, .devDependencies, .peerDependenciesMeta)' package.json > package.json.tmp
    mv package.json.tmp package.json
  '';

  npmDepsHash = "sha256-Wv0PFQeRVPlZvq+noluKWlt8uBvZnjAsCGxT8m+nPas=";

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r *.ts *.json node_modules skills $out/
    runHook postInstall
  '';

  passthru = {
    piExtension = {
      pname = finalAttrs.pname;
      version = finalAttrs.version;
    };
    updateScript = nix-update-script {
      extraArgs = [
        "--version-regex=v(.*)"
      ];
    };
  };

  meta = {
    description = "Run AI coding agents in pi TUI overlays with interactive shell";
    homepage = "https://github.com/nicobailon/pi-interactive-shell";
    license = lib.licenses.mit;
  };
})
