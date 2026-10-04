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
  version = "3.3.0-unstable-2026-10-04";

  src = fetchFromGitHub {
    owner = "dnouri";
    repo = "pi-coding-agent";
    rev = "b8ab7fcdb2f7e177133200acbcd9c85b03f1c052";
    sha256 = "sha256-fE/lxuFHuonVfIzWTjOXyQQUpG2cCirt6DKKMfhRfCk=";
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
