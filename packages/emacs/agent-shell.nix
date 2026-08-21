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
  version = "0.74.2-unstable-2026-08-21";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "0306939b918df3e692c18ce5aeb5fa19457d9893";
    sha256 = "sha256-AevH6lUFlLVMVk/we5R+kT6MoaXPZti70ycYYhWuQj8=";
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
