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
  version = "0.50.1-unstable-2026-04-29";

  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "agent-shell";
    rev = "85bd9c41d43a92b0961960864830dc893120c86d";
    sha256 = "sha256-rTkYLpDoRfRjQt9fvp7WZL8NPZ86cy751SwOyB+KMfA=";
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
