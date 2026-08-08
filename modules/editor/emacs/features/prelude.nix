{
  thisFeature,
}:
{ options, ... }:
{
  # Make this Elisp config come first in generated default.el.
  order = options.features.valueMeta.attrs.${thisFeature}.configuration.options.order.default - 10000;

}
