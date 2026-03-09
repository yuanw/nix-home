{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  shell-maker,
  ...
}:

melpaBuild {
  pname = "agent-shell";
  version = "0.0.1-unstable-2026-03-08";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "07f9130993d41a969fbe8ba23028fc8ad1855f07";
    sha256 = lib.fakeSha256;
  };

  packageRequires = [ shell-maker ];

  recipe = writeText "recipe" ''
    (agent-shell
     :repo "xenodium/agent-shell"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "AI agent shell for Emacs";
    homepage = "https://github.com/xenodium/agent-shell";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
