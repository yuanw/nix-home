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
  version = "0.9.9.4-unstable-2026-04-10";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "8d6411b5f89d796c817ff79324973b8910e164fe";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-BgOtog94HH/SRV1NRxv3tOu7rhxVXN0UZAhinG7DhX4=";
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
