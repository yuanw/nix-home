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
  version = "0.9.9.6-unstable-2026-09-06";

  src = fetchFromGitHub {
    owner = "karthink";
    repo = "gptel";
    rev = "4799c8018835f2e1af3da89513d7904dcc7a56f8";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-YD7rmktv1+Rvm89q9cCFz6JxlLHGTE87NQoosw9Shpo=";
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
