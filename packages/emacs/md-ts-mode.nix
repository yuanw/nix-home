{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "md-ts-mode";
  version = "0.3.0-unstable-2026-09-04";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "md-ts-mode";
    rev = "e593fdeef5b8a9805c33af1f35041b5d501398ee";
    sha256 = "sha256-C+Bv2nNTujkCLbUVTbiScyvqjjWrt8YWE0N/Jn3krwc=";
  };

  packageRequires = [ ];

  recipe = writeText "recipe" ''
    (md-ts-mode
     :repo "dnouri/md-ts-mode"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Markdown mode using tree-sitter";
    homepage = "https://github.com/dnouri/md-ts-mode";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
