{
  lib,
  myDefvar,
  ...
}:

let
  vars = import ../lib/elisp-vars.nix { inherit lib; };
in
{
  # Make this Elisp config come first in generated default.el.
  order = -100;

  # Packages/features that later modules can assume are available.
  epkgs = epkgs: [
    epkgs.use-package
  ];

  # This feature needs access to Nix values, so use `elisp` instead of
  # `elispFile`.  nima forbids setting both for one feature.
  elisp = ''
    ${vars.mkDefvars myDefvar}

    ${builtins.readFile ./prelude.el}
  '';
}
