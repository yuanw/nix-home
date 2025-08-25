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
  version = "0-unstable-2025-08-24";

  src = fetchFromGitHub {
    owner = "manzaltu";
    repo = "claude-code-ide.el";
    rev = "dca95da8e395331b6b0e9e186f6f7e8125efc9f9";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-0jZPcrWpEzsnqqTlQQZzjxMs6M3WpZFlREVl4YRMScY=";
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
