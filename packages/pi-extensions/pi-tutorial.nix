{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "fd3d840fc7e900b9df745f7c93ef2412a1c930dc";
in
mkPiExtension {
  pname = "pi-onboarding-guide.ts";
  version = "unstable-2026-04-13";
  src = fetchFromGitHub {
    owner = "earendil-works";
    repo = "pi-tutorial";
    inherit rev;
    hash = "sha256-GATInbM4wXILOZsYKtggYLZAY3h8uPb6QM0/Zq+U+3w=";
  };
  tsPath = "pi-onboarding-guide.ts";
}
