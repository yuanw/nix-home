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
  version = "0.9.9.4-unstable-2026-04-06";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "cd4375a53525a399f64adfc99c791b35c890d152";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-3pV1ITVviBF8+GQuMYMXo1vj3UPsAOCbtJXjkyq2GnA=";
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
