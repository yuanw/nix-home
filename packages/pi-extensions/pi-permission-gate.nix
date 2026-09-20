# nix-prefetch-github rytswd pi-agent-extensions --rev <rev>
{
  fetchFromGitHub,
  lib,
  stdenvNoCC,
  ...
}:
let
  rev = "268655eee9ba1256eef79bcc0e113915231461fc";
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "permission-gate";
  version = "0-unstable-2026-07-24";

  src = fetchFromGitHub {
    owner = "rytswd";
    repo = "pi-agent-extensions";
    inherit rev;
    hash = "sha256-1RWkjy/mQew1l67DgEj6ChNwkeuz/Kxn/FyymQGWSA0=";
  };

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    for f in permission-gate/*.ts; do
      base=$(basename "$f")
      case "$base" in
        *.test.ts|test-util.ts) ;;
        *) cp "$f" "$out/" ;;
      esac
    done
    runHook postInstall
  '';

  passthru.piExtension = {
    pname = finalAttrs.pname;
    version = finalAttrs.version;
  };

  meta = {
    description = "Prompt or block dangerous bash commands before they run (pi permission gate)";
    homepage = "https://github.com/rytswd/pi-agent-extensions/tree/main/permission-gate";
    license = lib.licenses.mit;
  };
})
