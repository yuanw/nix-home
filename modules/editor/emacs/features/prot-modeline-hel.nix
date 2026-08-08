{ lib, ... }:

let
  feature = import ../lib/feature.nix { inherit lib; };
in
feature.mkElispFeature {
  name = "prot-modeline-hel";
  file = ../configs/prot-modeline-hel.el;

  # Load after both `prot-modeline' and `hel' feature files have contributed
  # their configuration to generated default.el.
  order = 100;
}
