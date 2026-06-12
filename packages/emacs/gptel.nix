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
  version = "0.9.9.5-unstable-2026-06-08";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "c2c9ef78120ff1121b7bcd575ba18809b9c23f48";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-DgLvFyvD8MYh61ome3JJq31SyjbOMHKtbgwklPjeQLs=";
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
