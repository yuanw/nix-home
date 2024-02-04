{ melpaBuild
, fetchFromGitHub
, writeText
, substituteAll

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
  version = "20240201.0";

in
melpaBuild {

  inherit pname version;

  src = fetchFromGitHub {
    owner = "manateelazycat";
    repo = "lsp-bridge";
    rev = "a4b2be85ceee88520b0aa7f16ed7b8550b8e1e03";
    hash = "sha256-pURcrzo0rWQrjxloReG6LuP+dBtUMuHd07uCNh4RqIc=";

  };

  dontConfigure = true;
  dontBuild = true;

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
