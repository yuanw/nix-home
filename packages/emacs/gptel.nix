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
  version = "0.9.9.6-unstable-2026-11-21";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "743173759fd7e545deaaf80e39d08ec441d6c8ee";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-ZoqWG1tfq8MUOLFX8t13IeMp2MCYcOoISbG9fdAUJLs=";
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
