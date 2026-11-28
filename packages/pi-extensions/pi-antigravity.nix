# nix-prefetch-github Rahularya01 pi-antigravity --rev <rev>
{
  fetchFromGitHub,
  fetchNpmDeps,
  buildNpmPackage,
  lib,
  jq,
  ...
}:
let
  version = "0.16.0-unstable-2026-11-27";

  src = fetchFromGitHub {
    owner = "Rahularya01";
    repo = "pi-antigravity";
    rev = "09c16ae49b6bcc9c93171e4b9ffdae2bd2fefedf";
    hash = "sha256-mT+PSs+J2PsGC+ZPSMPLndbs17F6PBrKinXZLOocqlw=";
  };

  # Upstream declares peer deps on the Pi packages (provided by the host Pi
  # runtime) and dev deps that are not needed at runtime. The fetcher reads
  # package.json/package-lock.json from the *unpatched* source, so it needs
  # the same fixups and therefore jq in its own PATH. The lockfile is
  # vendored: upstream is yarn-based and pins undici 8.12.4 via yarn resolutions.
  dropUnresolvedDeps = ''
    cp ${./pi-antigravity.package-lock.json} package-lock.json
    jq 'del(.peerDependencies, .devDependencies) | .dependencies.undici = "8.12.4"' package.json > package.json.tmp
    mv package.json.tmp package.json
  '';

  npmDepsHash = "sha256-IUBE/RJayC1EInzKTHs9N/+bf/53HhXGEORB+nY2xm4=";
in
buildNpmPackage {
  pname = "pi-antigravity";
  inherit version src;

  nativeBuildInputs = [ jq ];

  npmDeps = fetchNpmDeps {
    name = "pi-antigravity-${version}-npm-deps";
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
    cp -r package.json src node_modules $out/
    cp LICENSE README.md $out/
    runHook postInstall
  '';

  passthru.piExtension = {
    pname = "pi-antigravity";
    inherit version;
  };

  meta = {
    description = "Antigravity / Cloud Code Assist provider for Pi Coding Agent";
    homepage = "https://github.com/Rahularya01/pi-antigravity";
    license = lib.licenses.mit;
  };
}
