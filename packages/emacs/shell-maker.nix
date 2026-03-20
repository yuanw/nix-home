{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.89.2-unstable-2026-03-18";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "55f829d179608a3c4b11e86427713d5be7c4bb58";
    sha256 = "sha256-tFzyVXxgrR6LOPUhX4MgJNLy3PX+pwprjdYi+IZXr+E=";
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
