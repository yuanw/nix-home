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
  version = "0.9.9.6-unstable-2026-09-19";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "44deae37928175764bbe8314dbbc79bdec28ab4c";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-eVkOeJge5W0duiD06JhcDWOmfA9V4fa4iX1sW6+EqVU=";
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
