{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.93.1-unstable-2026-06-09";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "43ee9e1862994cbaa89715d324edb7a424181f22";
    sha256 = "sha256-nJNZBZKmzEVn5gNEL0DT4S09Hfd8hk6DLaZTtoRksS0=";
  };

  packageRequires = [ ];

  recipe = writeText "recipe" ''
    (shell-maker
     :repo "xenodium/shell-maker"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "A shell maker library for Emacs";
    homepage = "https://github.com/xenodium/shell-maker";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
