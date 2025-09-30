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
  version = "0.9.9-unstable-2025-09-27";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "734541d23de6a2747a24c12e33a3ecfff62f275f";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-469L7sT5lmNPW8qaEexc77a2bwWPl4/RnEO6s22OZDk=";
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
