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
  version = "0.83.5-unstable-2026-10-04";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "1cd4f20e0ebbebe72829f163b1447cf432587c17";
    sha256 = "sha256-NU1it1xzSQyh8yH9JWPJCCjq4UiddJXCBqidO+csi3I=";
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
