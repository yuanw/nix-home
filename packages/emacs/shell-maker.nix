{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.97.2-unstable-2026-08-20";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "ab4f8ebaf4ef7a7db4762c5d5075baea580044ba";
    sha256 = "sha256-OT2aGY/7fc1c0qAqAMp7L7TDJSdRRniBWjNv3WmPMYc=";
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
