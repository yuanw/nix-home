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
  version = "0.9.9.5-unstable-2026-08-21";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "33b7af7113580c32af7a8593766d33f16b926973";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-U6kulpl2zgSdW7eedPKcmN8WLqgB7reMmYvLs4+YW08=";
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
