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
  version = "0.9.9.3-unstable-2025-12-16";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "591101dee3aaeb365b0c30e179c61bc9cb494781";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-gCELweLLPH3w/rJ1sI08BLHjEOxmtF3a9vbMYw6tKBs=";
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
