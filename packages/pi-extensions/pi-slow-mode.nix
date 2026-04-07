# nix-prefetch-github rytswd pi-agent-extensions --rev <rev>
{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "62d75ddac5c00155f34a2d94c105dfa932d4f9c6";
in
mkPiExtension {
  pname = "pi-slow-mode.ts";
  version = "0-unstable-2026-04-04";
  src = fetchFromGitHub {
    owner = "rytswd";
    repo = "pi-agent-extensions";
    inherit rev;
    hash = "sha256-WfL29tRXXRjFZY8/IbuypYrtmx0Bj757vuL2py0nuBY=";
  };
  tsPath = "slow-mode/index.ts";
}
