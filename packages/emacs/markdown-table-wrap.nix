{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "markdown-table-wrap";
  version = "0.2.0-unstable-2026-08-06";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "markdown-table-wrap";
    rev = "f846b77d13f34fba57c80214c1a61e00c94048a3";
    sha256 = "sha256-MEP7sxDpiOYbEms8MVRYr91TjpVj+PMdRTSDptUjqM4=";
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
