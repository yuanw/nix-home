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
  version = "persistent-prompt-via-shell-maker-snapshot-unstable-2026-09-24";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "19e9d0175a5392af9999447f6be99f65e43f37bb";
    sha256 = "sha256-/z16NhX9msJRDowFUZlfULYL87HPX4v9AIeS+RtN+io=";
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
