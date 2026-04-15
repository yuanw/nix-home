{ fetchFromGitHub, mkPiExtension, ... }:
let
  rev = "ed6db501d09a3a72ac5ab8ca604effa0ddcb65d5";
in
mkPiExtension {
  pname = "earendil-pi-review.ts";
  version = "unstable-2026-04-14";
  src = fetchFromGitHub {
    owner = "earendil-works";
    repo = "pi-review";
    inherit rev;
    hash = "sha256-DOq6YtyCKJXAOjKWP2/mSEzycMqeT0ZTk+RTTeix06Y=";
  };
  tsPath = "review.ts";
}
