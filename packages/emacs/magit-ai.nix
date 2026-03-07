{ melpaBuild
, fetchFromGitHub
, # Elisp dependencies
  magit ? null
, # Native dependencies
  ...
}:
melpaBuild {
  pname = "magit-ai";
  version = "0-unstable-2026-03-06";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "magit-ai";
    rev = "6afd1e5da0c93536efe3188e1b80c4c99f999cbe";
    sha256 = "sha256-slz7AgHdSOVhcaLuA1nEtIQhhLP3swMxh/klb7gPKk8=";
  };

  packageRequires = [ magit ];
  preferLocalBuild = true;
  allowSubstitutes = false;
}
