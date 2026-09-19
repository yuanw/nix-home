{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "md-ts-mode";
  version = "0.4.0-unstable-2026-09-15";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "md-ts-mode";
    rev = "f9030a0765f1cbb24ba24c05570ca3cf25769d17";
    sha256 = "sha256-IpKNWQAmBTC0Nrn165spUoJgnHwNI+IorqyyUiw7H3E=";
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
