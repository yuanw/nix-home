{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "md-ts-mode";
  version = "0.5-unstable-2026-10-04";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "md-ts-mode";
    rev = "769ef52965c46e9346bf19d04adc6fef9d23b01c";
    sha256 = "sha256-YhRk6lhFm7zVicP/c8228NZzpMcSIyrFLTH2SZ4tAEs=";
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
