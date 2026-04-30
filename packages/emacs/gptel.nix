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
  version = "0.9.9.4-unstable-2026-04-28";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "71b9f9414536c4cd35d8c02d3c8ad4fc4aea8718";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-VHRRHgBkC3ieepbL6wM+L1rTz9UPfQji2DFsGEOzXyk=";
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
