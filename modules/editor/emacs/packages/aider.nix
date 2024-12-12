{
  melpaBuild,
  fetchFromGitHub,
  writeText,
  unstableGitUpdater,
  # Elisp dependencies

  # Native dependencies
  ...
}:

let
  pname = "aider";
  version = "0.0.1";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "tninja";
    repo = "aider.el";
    rev = "515f5cbb505e614f400c21f99bdbebd47609c659";
    #sha256 = lib.fakeSha256;
    sha256 = "sha256-NTkpiG0w/Bmo2CjlCLIA/6+feWcDQ/+IGPNy3Ml/AbA=";
  };

  dontConfigure = true;
  dontBuild = true;
  passthru.updateScript = unstableGitUpdater { };

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (aider
     :repo "tninja/aider.el"
     :files ("*.el")
     :fetcher github)
  '';

  packageRequires = [

  ];
}
