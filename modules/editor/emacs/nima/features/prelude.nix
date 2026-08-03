{
  thisFeature,
}:
{ options, ... }:
{
  # Make this Elisp config come first in generated default.el.
  order = options.features.valueMeta.attrs.${thisFeature}.configuration.options.order.default - 10000;

  # This is also nima's default for ./prelude.nix, but keeping it explicit makes
  # it obvious that ./prelude.el is included in generated default.el.
  elispFile = ./prelude.el;

  # Packages/features that later modules can assume are available.
  epkgs = epkgs: [
    epkgs.use-package
  ];

}
