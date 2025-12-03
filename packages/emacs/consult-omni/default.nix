{
  melpaBuild,
  fetchFromGitHub,
  writeText,

  # Elisp dependencies

  consult ? null,
  elfeed ? null,
  embark ? null,
  browser-hist ? null,
  consult-notes ? null,
  yequake ? null,
  ...

}:

let
  pname = "consult-omni";
  version = "0.1-unstable-2025-11-28";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "armindarvish";
    repo = "consult-omni";
    rev = "23fc72faecb2161066832bb30e6dcf22690a30f1";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-8qJTvt0DdESgXU9fyAjzKPQw21D9s0tY2Wa7rIvQAeI=";
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (consult-omni
    :repo "armindarvish/consult-omni"
    :files ("*.el" "sources"  )
    :fetcher github
  '';
  propagatedUserEnvPkgs = [
    yequake
  ];
  buildInputs = [ yequake ];
  packageRequires = [
    consult
    consult-notes
    elfeed
    embark
    browser-hist
    yequake
  ];
}
