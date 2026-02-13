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
  version = "0.9.9.3-unstable-2026-02-13";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "8879956648cc40e27c624ec6221971b7ab79d457";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-qTQhhxpHvm/nOqx5b8ADzx+FFuIsEcGMJUDFZPsdMeM=";
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
