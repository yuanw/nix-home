# nix-prefetch-github badlogic pi-mono --rev <rev>
{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "v0.64.0";
in
mkPiExtension {
  pname = "pi-interactive-shell.ts";
  version = "0.64.0";
  src = fetchFromGitHub {
    owner = "badlogic";
    repo = "pi-mono";
    inherit rev;
    hash = "sha256-knCfmoTjq5RADkGRcX7AAxTBhW+2GL4pDtgvMH8pMoY=";
  };
  tsPath = "packages/coding-agent/examples/extensions/interactive-shell.ts";
}
