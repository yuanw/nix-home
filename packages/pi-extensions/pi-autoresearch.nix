# nix-prefetch-github davebcn87 pi-autoresearch --rev <rev>
{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "ad2eed37743fcd51a13ef8945e8ee05a649c59ba";
in
mkPiExtension {
  pname = "pi-autoresearch.ts";
  version = "0.0.0-unstable-2026-04-06";
  src = fetchFromGitHub {
    owner = "davebcn87";
    repo = "pi-autoresearch";
    inherit rev;
    hash = "sha256-4YAUMIw9VP5nw/23ozNIWpRvsQk3RMwGNlLEr6yPeFo=";
  };
  tsPath = "extensions/pi-autoresearch/index.ts";
}
