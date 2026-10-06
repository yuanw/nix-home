{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  fetchNpmDeps,
  jq,
  nix-update-script,
  ...
}:
let
  version = "0.17.0";

  src = fetchFromGitHub {
    owner = "nicobailon";
    repo = "pi-interactive-shell";
    rev = "v${version}";
    hash = "sha256-zugoEOJNhjZ7GeGj+Ak6tptmxd2y7/pH5/1jmj1iG2w=";
  };

  # Upstream package-lock.json resolves the @earendil-works/pi-* peer/dev deps
  # from git urls, which the npm fetcher cannot download. Drop them from
  # package.json and use a lockfile regenerated against registry.npmjs.org:
  #   jq 'del(.peerDependencies, .devDependencies, .peerDependenciesMeta)' package.json \
  #     | sponge package.json && rm -f package-lock.json \
  #     && npm install --package-lock-only --ignore-scripts --registry=https://registry.npmjs.org/
  dropUnresolvedDeps = ''
    cp ${./pi-interactive-shell.package-lock.json} package-lock.json
    jq 'del(.peerDependencies, .devDependencies, .peerDependenciesMeta)' package.json > package.json.tmp
    mv package.json.tmp package.json
  '';

  npmDepsHash = "sha256-Wv0PFQeRVPlZvq+noluKWlt8uBvZnjAsCGxT8m+nPas=";
in
buildNpmPackage {
  pname = "pi-interactive-shell";
  inherit version src;

  nativeBuildInputs = [ jq ];

  # fetchNpmDeps reads package.json/package-lock.json from the *unpatched*
  # source, so it needs the same fixups and therefore jq in its own PATH.
  npmDeps = fetchNpmDeps {
    name = "pi-interactive-shell-${version}-npm-deps";
    inherit src;
    hash = npmDepsHash;
    nativeBuildInputs = [ jq ];
    postPatch = dropUnresolvedDeps;
  };

  postPatch = dropUnresolvedDeps;

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r *.ts *.json node_modules skills $out/
    runHook postInstall
  '';

  passthru = {
    piExtension = {
      pname = "pi-interactive-shell";
      inherit version;
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
}
