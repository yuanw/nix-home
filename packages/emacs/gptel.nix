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
  version = "0.9.9.3-unstable-2025-11-18";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "9d6313ce7c6eb71bfb24a8d1edcd60fe4534eea3";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-mBInxLzXu2t4DH+TeAX7AsLFm9zz/bzD8g4QMtm0aZg=";
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
