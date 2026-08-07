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
  version = "2.7.0-unstable-2026-08-05";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pi-coding-agent";
    rev = "60c53bde4500214db0afb7495c20c634bb90f7fe";
    sha256 = "sha256-mOPiBSWzx0S2veMlj2b2VfUMKf9r/w9Gs7yXTXF4Mx8=";
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
