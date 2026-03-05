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
  version = "0.9.9.4-unstable-2026-03-04";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "2a201016ff954304679ac1f1c81ea7bd78f507ae";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-Vpyv2fZTS3VJHX8KuzEVtlWSK4iSBtv9q2a1Nc9SUTk=";
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
