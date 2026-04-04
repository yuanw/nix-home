{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "whisper";
  version = "0.1.5-unstable-2025-01-10";

  src = fetchFromGitHub {
    owner = "natrys";
    repo = "whisper.el";
    rev = "fd9bf5787a99dd31a4bdf54d2bd9821aacf84e93";
    sha256 = "sha256-Rbb6uVBKk/LdsSmMF/R7VFalJuyddiLZYfrsE4IuRX0=";
  };

  recipe = writeText "recipe" ''
    (whisper
     :repo "natrys/whisper.el"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Speech-to-text for Emacs via whisper.cpp";
    homepage = "https://github.com/natrys/whisper.el";
    license = licenses.gpl3Only;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
