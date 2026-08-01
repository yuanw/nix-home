{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  ...
}:

melpaBuild {
  pname = "shell-maker";
  version = "0.93.5-unstable-2026-07-27";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "shell-maker";
    rev = "679cfbc02e206e0a702048cfd7c663eb5c9d1059";
    sha256 = "sha256-oy5nS/6/pnghwk44bW+75oaaVMX4JqMsxcU5hunDoNc=";
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
