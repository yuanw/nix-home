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
  version = "0.9.9.6-unstable-2026-11-17";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "0b0aad0b19f8f0bc86ea8cfb1dbe2f02b4ea43de";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-QH+hLcY9P0geOKjGb+aOwtlgUl/i5b5WNGBOaFpbbio=";
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
