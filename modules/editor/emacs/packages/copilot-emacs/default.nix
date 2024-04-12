{ fetchFromGitHub
, # Elisp dependencies
  dash
, editorconfig
, s
, f
, jsonrpc
, # Native dependencies
  nodejs
, trivialBuild
, lib
}:
trivialBuild {
  pname = "copilot";
  version = "unstable-2024-04-05";
  src = fetchFromGitHub {
    owner = "copilot-emacs";
    repo = "copilot.el";
    rev = "5f30a2b667df03c120ba31ce3af933255c8a558b";
    sha256 = "sha256-28pTCggQyVIn5pA260VokYjH5kaypBrG3FDphfzXJcU=";
  };
  # TODO wrap nodejs with copilot dependencies, just like in lsp-brigt
  packageRequires = [
    dash
    editorconfig
    nodejs
    s
    f
    jsonrpc
  ];

  meta = {
    description = "An unofficial copilot plugin for Emacs";
    homepage = "https://github.com/zerolfx/copilot.el";
  };
}
