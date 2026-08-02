{
  ...
}:

{
  # Make this Elisp config come first in generated default.el.
  order = -100;

  # Packages/features that later modules can assume are available.
  epkgs = epkgs: [
    epkgs.use-package
  ];

}
