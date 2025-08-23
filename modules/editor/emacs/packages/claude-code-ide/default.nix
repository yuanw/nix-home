{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  websocket,
  transient,
  web-server,
  vterm,
  eat,
  flycheck,
  ...
}:

melpaBuild {
  pname = "claude-code-ide";
  version = "0-unstable-2025-08-17";

  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "907f28ed82b743b0fff945ebe772d10953d78bfe";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-nRe3cCF3EtK8zyuqwguzUotDH/cqoNedIqEM5HXjC/4=";
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
