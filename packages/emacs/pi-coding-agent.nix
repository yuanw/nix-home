{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  transient,
  md-ts-mode,
  markdown-table-wrap,
  ...
}:

melpaBuild {
  pname = "pi-coding-agent";
  version = "3.1.0-unstable-2026-09-23";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pi-coding-agent";
    rev = "7ca4ee574d6a2b14032a9769c5c4cada97b97977";
    sha256 = "sha256-I84N9Ax3Jm3xtojs7+TVyMn4eUDszfv91xVNvpPaZhQ=";
  };

  packageRequires = [
    transient
    md-ts-mode
    markdown-table-wrap
  ];

  recipe = writeText "recipe" ''
    (pi-coding-agent
     :repo "dnouri/pi-coding-agent"
     :fetcher github
     :files ("pi-coding-agent.el"
              "pi-coding-agent-core.el"
              "pi-coding-agent-grammars.el"
              "pi-coding-agent-input.el"
              "pi-coding-agent-menu.el"
              "pi-coding-agent-render.el"
              "pi-coding-agent-table.el"
              "pi-coding-agent-ui.el"))
  '';

  meta = with lib; {
    description = "Emacs frontend for the Pi coding agent";
    homepage = "https://github.com/dnouri/pi-coding-agent";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
