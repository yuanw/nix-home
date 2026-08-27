# nix-prefetch-github helheim-emacs hel-collection --rev <rev>
{
  fetchFromGitHub,
  melpaBuild,
  writeText,
  lib,
  hel,
  dash,
  ...
}:

let
  version = "0.2.0-unstable-2026-08-26";
  rev = "b8926eccfad6c4184186b6936d244acb9653f3d3";
in

melpaBuild {
  pname = "hel-collection";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel-collection";
    inherit rev;
    hash = "sha256-jwBe+V8FHNV6gqz4RBbWV0kLtsjuYeJJgV53qi0w0c0=";
  };

  packageRequires = [
    hel
    dash
  ];

  recipe = writeText "recipe" ''
    (hel-collection
     :repo "helheim-emacs/hel-collection"
     :fetcher github
     :files ("hel-collection.el" "modes"))
  '';

  meta = with lib; {
    description = "Hel keybindings for third-party Emacs packages";
    homepage = "https://github.com/helheim-emacs/hel-collection";
    license = licenses.gpl3Only;
    platforms = platforms.all;
  };
}
