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
  version = "0-unstable-2025-12-14";

  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "3ad91302e16615bc42d4d69de8ff1387dedac57f";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-2oF7RNBZKfUwU4dYAWJ78DJiNrEj2+c3ugRWMh3QbjY=";
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
