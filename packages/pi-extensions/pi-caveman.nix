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
    hash = "sha256-04qn0p22355qx8rq1j3cpy6fzb389hx9ssngsn6av15z836f1fq1=";
  };
  tsPath = "extensions/caveman.ts";
}
