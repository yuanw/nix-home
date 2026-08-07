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
  version = "0.9.9.5-unstable-2026-08-05";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "fc6963634af2f76a9909ad674e2c0b3f005e60b5";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-wAwcYNueadFol8poYhQyChlaqEuUlGrB1Wvq7ERWINk=";
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
