{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "markdown-table-wrap";
  version = "0.2.0-unstable-2026-07-03";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "markdown-table-wrap";
    rev = "afc8214c6a2109891c5adf5ee7f75b8d8a2c4a35";
    sha256 = "sha256-b1DuNOoWp7U8w3iJt/SYPJOSb6Q4nRqCkde2NbK5QVg=";
  };

  packageRequires = [ ];

  recipe = writeText "recipe" ''
    (markdown-table-wrap
     :repo "dnouri/markdown-table-wrap"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Wrap markdown table cells for display";
    homepage = "https://github.com/dnouri/markdown-table-wrap";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
