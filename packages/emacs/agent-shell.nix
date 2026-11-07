{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  shell-maker,
  acp,
  ...
}:

melpaBuild {
  pname = "agent-shell";
  version = "0.91.1-unstable-2026-11-06";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "4564c5d27cff73eceea6d7aec44f4fc175d6f6f5";
    sha256 = "sha256-H53VEzwRIYx4j8dbwUE9lQgr8lcLFzCEXmwSxvGQkH4=";
  };

  packageRequires = [
    shell-maker
    acp
  ];

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
