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
  pname = "hurl-mode";
  version = "0-unstable-2025-02";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "JasZhe";
    repo = "hurl-mode";
    rev = "df03471e48fb1ca39050d23d61c79ae901e8d68f";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-EauCBuqIFhvY5e0lPn9sFFo4K918Rc6Xx9myZTaAXxc=";
  };

  dontConfigure = true;
  dontBuild = true;
  passthru.updateScript = unstableGitUpdater { };

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (hurl-mode
    :repo "jaszhe/hurl-mode"
    :files ("*.el")
    :fetcher github)
  '';

  packageRequires = [

  ];
}
