{
  melpaBuild,
  fetchFromGitHub,
  writeText,

  # Elisp dependencies

  # Native dependencies
  ...
}:

let
  pname = "home-row-expreg";
  version = "0-unstable-2025-09-22";
in
melpaBuild {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "bommbo";
    repo = "home-row-expreg";
    #https://github.com/casouri/expreg/pull/9
    rev = "3b371bc26544d25917bdd3bbf9ba14a13092aa84";

    #sha256 = lib.fakeSha256;

    sha256 = "sha256-0JBTVCfKsu80tvZhXD0E9sYi1TEbbnv0caza3WOCVg8=";

  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/$pname-$version
    cp -rv * $out/share/emacs/site-lisp/elpa/$pname-$version/
  '';

  recipe = writeText "recipe" ''
    (expreg
    :repo "bommbo/home-row-expreg"
    :files ("*.el")
    :fetcher github)
  '';

  packageRequires = [

  ];
}
