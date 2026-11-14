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
  version = "0-unstable-2026-08-31";
  rev = "7d0d050fd555b3ac187e2754c0b2d327256a58ab";
in

melpaBuild {
  pname = "hel-collection";
  inherit version;

  src = fetchFromGitHub {
    owner = "helheim-emacs";
    repo = "hel-collection";
    inherit rev;
    hash = "sha256-TeTyoWSeUur9LaTYWJ/w1CE1Ze+yuwVGBl1wzCICTd0=";
  };

  packageRequires = [
    hel
    dash
  ];

  # Upstream defines the agent-shell leader keymaps after the
  # `hel-collection-setup' form that binds them (still true on main), so
  # loading that file when agent-shell is already loaded evaluates the
  # bindings before the keymaps exist and dies with
  # "void-variable hel-agent-shell-local-leader-map".  Defining them first
  # makes the file load in any order.
  # shortcut: local patch; if upstream reshuffles this file the patch stops
  # applying and the build fails loudly on the next rev bump.
  patches = [ ./hel-collection-agent-shell-order.patch ];

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
