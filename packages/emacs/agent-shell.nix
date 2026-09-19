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
  version = "persistent-prompt-via-shell-maker-snapshot-unstable-2026-09-19";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "e78b43487007d59415d34c3cec4fadf814b8a373";
    sha256 = "sha256-tc3BUs8dQXM3UaF16wtcd3b2rreailj1wu2rh1Mn32c=";
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
