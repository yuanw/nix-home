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
  version = "0.9.9.5-unstable-2026-07-31";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "9cafa41e7172a995e4eeadb5049a8c05bf7c30ba";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-wFR4qFXzJcWMfVz2GSN15m93JSAkloBcBUijHigszHo=";
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
