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
  version = "0.9.9.3-unstable-2025-11-28";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "73144b7345693b046174364edb68e1a5f5a3c7ed";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-LudCczrQpHNPw1K9/YzeLEcTDaSmM08y1q2F+aD/bi4=";
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
