{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  agent-shell,
  ...
}:

melpaBuild {
  pname = "agent-shell-manager";
  version = "0-unstable-2026-03-16";

  src = fetchFromGitHub {
    owner = "jethrokuan";
    repo = "agent-shell-manager";
    rev = "53b73f13ed1ac9d2de128465a8504a7265490ea7";
    sha256 = "sha256-JPB/OnOhYbM0LMirSYQhpB6hW8SAg0Ri6buU8tMP7rA=";
  };

  packageRequires = [
    agent-shell
  ];

  recipe = writeText "recipe" ''
    (agent-shell-manager
     :repo "jethrokuan/agent-shell-manager"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Tabulated buffer manager for agent-shell";
    homepage = "https://github.com/jethrokuan/agent-shell-manager";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
