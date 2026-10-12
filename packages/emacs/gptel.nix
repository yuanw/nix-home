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
  version = "0.9.9.6-unstable-2026-10-12";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "cd372be78471f948c3c2fddf652e511e39932f7c";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-yGWIZWHB+rU1Ik2iueT7uCwmLFmVeV094CPKWlC3YZY=";
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
