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
  version = "0-unstable-2025-02-19";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "armindarvish";
    repo = "consult-omni";
    rev = "d0a24058bf0dda823e5f1efcae5da7dc0efe6bda";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-dzKkJ+3lMRkHRuwe43wpzqnFvF8Tl6j+6XHUsDhMX4o=";
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
