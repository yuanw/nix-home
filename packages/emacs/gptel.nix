{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  compat ? null,
  transient ? null,
  ...
}:

melpaBuild {
  pname = "gptel";
  version = "0-unstable-2026-05-05";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "df08c1a4d4c6e253870b8aa1634922b8f56224e7";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-VHTJcWxxrlZblMLwsqNyYDRhwukudGFf8KT/MWk2bgQ=";
  };

  packageRequires = [
    compat
    transient
  ];

  recipe = writeText "recipe" ''
    (gptel
     :repo "karthink/gptel"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "A simple LLM client for Emacs";
    homepage = "https://github.com/karthink/gptel";
    license = licenses.gpl3Only;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
