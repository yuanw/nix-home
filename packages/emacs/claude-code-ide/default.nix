{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  websocket ? null,
  transient ? null,
  web-server ? null,
  vterm ? null,
  eat ? null,
  flycheck ? null,
  ...
}:

melpaBuild {
  pname = "claude-code-ide";
  version = "0-unstable-2026-02-02";

  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-tivRvgfI/8XBRImE3wuZ1UD0t2dNWYscv3Aa53BmHZE=";
  };

  packageRequires = [
    websocket
    transient
    web-server
    vterm
    flycheck
    eat
  ];

  recipe = writeText "recipe" ''
    (claude-code-ide
     :repo "manzaltu/claude-code-ide.el"
     :fetcher github
     :files ("*.el"))
  '';

  meta = with lib; {
    description = "Claude Code IDE for Emacs provides native integration with Claude Code CLI through the Model Context Protocol (MCP)";
    homepage = "https://github.com/manzaltu/claude-code-ide.el";
    license = licenses.gpl3Only;
    maintainers = [ ];
    platforms = platforms.all;
  };
}
