# nix-prefetch-github jonjonrankin pi-caveman --rev <rev>
{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "e5f329446ad1ff747eab381900c44810038b7c0f";
in
mkPiExtension {
  pname = "caveman.ts";
  version = "1.0.0-unstable-2026-04-22";
  src = fetchFromGitHub {
    owner = "jonjonrankin";
    repo = "pi-caveman";
    inherit rev;
    hash = "sha256-AbvgzEC/hK2M1c9qnTpMaKzvjL9syIAz6riUIcQFFhM=";
  };
  tsPath = "extensions/caveman.ts";
}
