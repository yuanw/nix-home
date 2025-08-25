{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  compat,
  transient,
  ...
}:

melpaBuild {
  pname = "gptel";
  version = "0-unstable-2025-08-22";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "90b41b1e45c91d2f02df886c6b86cc8bdb52090a";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-39wavfSi8GsodRFZnNG4Rfp0JUJL0Z62NiQOxzsrnGA=";
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
