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
  version = "0.9.9.5-unstable-2026-06-16";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "983cb1fece13b245b99da252b396cc163f591b09";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-/9CoOa6CpGrxNQUSqW4nSOzxacDhRjCdf9C5uZ/C50U=";
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
