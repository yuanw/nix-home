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
  version = "0.9.9.6-unstable-2026-09-18";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "1230375331c911d721b54a54d6a30d1ae0283787";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-4BhQ3hBCmp2jYMMdyXgKt6qogFI9fiR6u4UslEPtQ8I=";
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
