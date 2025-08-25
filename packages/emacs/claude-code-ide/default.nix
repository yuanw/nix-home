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
  version = "0-unstable-2025-08-25";

  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "76d83f30fd39b8d4b16b7f2b185e1e3f74cab227";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-9qj9BEkaB/AFRzNavdaYsdnW3w2ZRMW1o+q9hjPL8D0=";
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
