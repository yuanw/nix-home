{
  thisFeature,
}:
{ options, ... }:
{
  # Make this Elisp config come first in generated default.el.
  order = options.features.valueMeta.attrs.${thisFeature}.configuration.options.order.default - 10000;
  # Packages/features that later modules can assume are available.
  epkgs = epkgs: [
    epkgs.use-package
  ];

}
