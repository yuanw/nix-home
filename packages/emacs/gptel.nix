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
  version = "0.9.9.4-unstable-2026-04-13";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "4747ffa251af84dbb6fe181f5d2b484ec2e442a7";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-Zxx2xEXf70SOASIDjfsTus7Ir91dxfsLkbU5x8ul5bg=";
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
