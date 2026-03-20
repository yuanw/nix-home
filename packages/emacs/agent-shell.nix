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
  version = "0.50.1-unstable-2026-03-20";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "de0db122d1d57d5fd7e2c1853faf411456382235";
    sha256 = "sha256-PxKt/HTLrpz5Ll7LqVTzp/WkPo+2IT5K5CjVonjcNuw=";
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
