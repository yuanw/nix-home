{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "md-ts-mode";
  version = "0.3.0-unstable-2026-07-03";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "md-ts-mode";
    rev = "95ae25162da092cb1d55d2be0c2c95e0591086c2";
    sha256 = "sha256-b7nP9fdwUsVW29U2NHIfOANDtrIl091mzv9EumZWV+s=";
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
