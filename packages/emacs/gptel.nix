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
  version = "0.9.9.4-unstable-2026-03-09";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "9682e8f5f96a72c858fb3420003642194793b32f";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-37JxAnfcXJt86ea8W7uqNc/7mAQkba3rFUzrVncLVmw=";
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
