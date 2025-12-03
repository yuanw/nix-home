{
  melpaBuild,
  fetchFromGitHub,
  writeText,

  # Elisp dependencies

  # Native dependencies
  ...
}:

let
  pname = "hurl-mode";
  version = "0-unstable-2025-12-02";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "JasZhe";
    repo = "hurl-mode";
    rev = "2344f7c4b15192e01ffbff6d763beba76af47467";
    # sha256 = lib.fakeSha256;
    sha256 = "sha256-LNCypwmtzdBhia6FnOHi52TkhL/coKNZW8LtKqR9g64=";
  };

  dontConfigure = true;
  dontBuild = true;

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
