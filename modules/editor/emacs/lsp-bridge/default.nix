{ melpaBuild
, fetchFromGitHub
, writeText
, substituteAll

  # Elisp dependencies

, markdown-mode
, yasnippet
, fakeHash

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
  version = "20240216.0";

in
melpaBuild {

  inherit pname version;

  src = fetchFromGitHub {
    owner = "manateelazycat";
    repo = "lsp-bridge";
    rev = "270e42d52c545f959a9e5c79e8f317f6cf4b2162";
    hash = fakeHash;

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
