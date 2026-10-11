# nix-prefetch-github rytswd pi-agent-extensions --rev <rev>
{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "798cf2859c3848bfa175652e00e2784fd98246b2";
in
mkPiExtension {
  pname = "pi-slow-mode.ts";
  version = "0-unstable-2026-09-22";
  src = fetchFromGitHub {
    owner = "rytswd";
    repo = "pi-agent-extensions";
    inherit rev;
    hash = "sha256-qmAihSg/IMvd6b98lKfclY/a2sZe6jEHIInvBB4nBMQ=";
  };
  tsPath = "slow-mode/index.ts";
}
