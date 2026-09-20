# nix-prefetch-github mitsuhiko agent-stuff --rev <rev>
{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "80e1e96fa563ffc0c9d60422eac6dc9e67440385";
in
mkPiExtension {
  pname = "pi-review.ts";
  version = "1.5.0-unstable-2026-03-30";
  src = fetchFromGitHub {
    owner = "mitsuhiko";
    repo = "agent-stuff";
    inherit rev;
    hash = "sha256-JKMqt5ionfF/aBFTSQe9BD49wAErNtEnf3Mnekk3nzk=";
  };
  tsPath = "pi-extensions/review.ts";
}
