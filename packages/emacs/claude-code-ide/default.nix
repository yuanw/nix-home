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
  version = "0-unstable-2025-09-15";

  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "32d853e20b9d245a6ee89c4a153a4e568250c62c";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-OrcnUZXqRijJCgf1QE5kkPKKdWSJ4oMYt47Sn/EdQy0=";
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
