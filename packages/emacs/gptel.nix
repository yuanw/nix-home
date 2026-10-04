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
  version = "0.9.9.6-unstable-2026-10-02";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "edb3fee3b5266e9060f6d121e9b3914eb7c3409d";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-t74SarljbYF8k2HEh632GgJOrmqa7ZK9GOLXYi9L+d4=";
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
