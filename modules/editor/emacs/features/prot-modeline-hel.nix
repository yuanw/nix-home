{
  thisFeature,
}:
{ options, ... }:
{
  # Load after both `prot-modeline' and `hel' have contributed configuration.
  order = options.features.valueMeta.attrs.${thisFeature}.configuration.options.order.default + 100;
}
