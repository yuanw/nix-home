{
  # We can use `elispFile` when we do not need to access the Nix world in our
  # Elisp config. This is also nima's default for ./prelude.nix, but keeping it
  # explicit documents the pattern from nima's simple example.
  elispFile = ./prelude.el;

  # Make this Elisp config come first in generated default.el.
  order = -100;

  # Packages/features that later modules can assume are available.
  epkgs = epkgs: [
    epkgs.use-package
  ];
}
