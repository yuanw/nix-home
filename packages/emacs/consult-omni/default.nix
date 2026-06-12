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
  version = "0.1-unstable-2025-12-04";

in
melpaBuild {

  inherit pname version;
  src = fetchFromGitHub {
    owner = "armindarvish";
    repo = "consult-omni";
    rev = "bdcd5a065340dce9906ac5c5f359906d31877963";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-vmKKEmZpzHQ8RDbTuoTCWGRypLfMiHrEv9Zw0G6K1pg=";
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
