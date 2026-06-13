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
  version = "0.9.9.5-unstable-2026-06-10";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "6589abf7c51143f9424f3e402b29dd7755408781";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-LnskwoXxVMrrWaHdnORVt8/nmb7zfiJ9aeby4Vu98vw=";
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
