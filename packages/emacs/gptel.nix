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
  version = "0.9.9.6-unstable-2026-11-05";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "e161ababf0cb19654ec9097f06c09c95d4561721";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-OQtjgQU0CxnlsUBcA80aOfYMRYrwzOh0hwfVtBMX6qY=";
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
