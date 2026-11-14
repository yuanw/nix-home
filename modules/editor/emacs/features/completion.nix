{
  epkgs =
    epkgs:
    builtins.attrValues {
      inherit (epkgs)
        orderless
        vertico
        marginalia
        corfu
        cape
        ;
    };
}
