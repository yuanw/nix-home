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
  version = "0.9.9.6-unstable-2026-10-18";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "e7cebe4621533a036840d47c9741682bb687d808";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-BjMRaNYelvHNYTeK6JdC4jiVqKwml70TkAD1AdITi70=";
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
