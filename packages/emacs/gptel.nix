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
  version = "0.9.8.5-unstable-2025-08-24";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "7bb353a582d11ce210bb0a5b77c53de564c28e67";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-5Zn+hFuEWi/lJA9/lvIyBrJsXm7pDjbGZjnb21TLC38=";
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
