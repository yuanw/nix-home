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
    rev = "c59aaf65e27c12ac4f45e11dd83ac75c2d5c46aa";
    hash = "sha256-ZgFSIR4nHgri7TpFLH1aVcD3vKHx3MSt4wSrv5KE13g=";
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
