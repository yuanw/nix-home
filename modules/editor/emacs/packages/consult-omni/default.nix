{ melpaBuild
, fetchFromGitHub
, writeText
, substituteAll
, unstableGitUpdater
, lib
  # Elisp dependencies

, consult
, embark


}:

let
  pname = "consult-omni";
  version = "0.0.1";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "armindarvish";
    repo = "consult-omni";
    rev = "6e214cb29cf98a23230bf3633a07d46a328d0245";
    sha256 = lib.fakeSha256;
    # sha256 = "sha256-ZG3ZhhqDtL7M7x5XRr8Qb6mFlFndMWq5QPbtYLblXcs=";
  };

  dontConfigure = true;
  dontBuild = true;
  passthru.updateScript = unstableGitUpdater { };

  installPhase = ''
    rm -r test
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (consult-omni
    :repo "armindarvish/consult-omni"
    :files ("*.el" "sources"  )
    :fetcher github
  '';

  packageRequires = [
    consult
    embark
  ];
}
