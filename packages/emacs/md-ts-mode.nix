{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "md-ts-mode";
  version = "0.4.0-unstable-2026-09-23";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "md-ts-mode";
    rev = "c6f0bc5cb505ac82802ef588a6a9bc085c823c2f";
    sha256 = "sha256-pvpHtFrZIOcIWh8P5Hy2Z9svbUeWrX+BNcTey+I3wAM=";
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
