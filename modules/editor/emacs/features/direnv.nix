{ ... }:

{
  epkgs = epkgs: [
    epkgs.direnv
  ];

  # With featuresDir this is also the default, but keeping it explicit makes the
  # migration pattern obvious for larger packages.
  elispFile = ./direnv.el;
}
