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
  version = "3.0.2-unstable-2026-09-07";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pi-coding-agent";
    rev = "894d1e7be124ecbdfcb203d9c22b627b61620ace";
    sha256 = "sha256-sy6eiCMpAr0IpRyleFKhcsPoij4/X+C8Sswh6q2Rad0=";
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
