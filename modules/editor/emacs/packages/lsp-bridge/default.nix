{ melpaBuild
, fetchFromGitHub
, writeText
, substituteAll
, unstableGitUpdater
, lib
  # Elisp dependencies

, markdown-mode
, yasnippet

  # Native dependencies
, python3

}:

let
  # epc orjson sexpdata six setuptools paramiko rapidfuzz

  pythonEnv = ((python3.withPackages (ps: [
    ps.epc
    ps.orjson
    ps.paramiko
    ps.rapidfuzz
    ps.sexpdata


  ])).override { ignoreCollisions = true; });


  pname = "lsp-bridge";
  version = "20240416.0";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "manateelazycat";
    repo = "lsp-bridge";
    rev = "0345cc6534c9ada91dbd93d58ea965ff542b1369";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-ZG3ZhhqDtL7M7x5XRr8Qb6mFlFndMWq5QPbtYLblXcs=";
  };

  dontConfigure = true;
  dontBuild = true;
  passthru.updateScript = unstableGitUpdater { };

  patches = [
    # Hardcode the python dependencies needed for lsp-bridge, so users
    # don't have to modify their global environment
    (substituteAll {
      src = ./hardcode-dependencies.patch;
      python = pythonEnv.interpreter;
    })
  ];


  installPhase = ''
    rm -r test
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (lsp-bridge
    :repo "manateelazycat/lsp-bridge"
    :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
    :fetcher github
  '';

  packageRequires = [
    markdown-mode
    yasnippet

  ];
}
