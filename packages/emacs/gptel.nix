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
  version = "0.9.9-unstable-2025-11-05";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "7c8d9c8496afa50bf03f2dd7fabeefe9449cf381";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-7EsaPTxLeC5OA/wyaEkSyMLDe69nwocNZ2yk0A3ZUCs=";
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
