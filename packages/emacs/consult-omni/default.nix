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
  version = "0.0.1";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "armindarvish";
    repo = "consult-omni";
    rev = "6e214cb29cf98a23230bf3633a07d46a328d0245";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-vAis6/U70PWuyl4NNg6uC5RfOFZYVy1g+nlvmi2f1jo=";
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
