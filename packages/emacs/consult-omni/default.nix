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
  version = "0.1-unstable-2026-06-25";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "armindarvish";
    repo = "consult-omni";
    rev = "3a126ee54479755408faed10da945dbc2366303b";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-8Koe6IO2+/HvDb2IE1dc/DY3hGmNWcEMAfz6d0gdT7k=";
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
