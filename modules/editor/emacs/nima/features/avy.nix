{ ... }:

{
  epkgs = epkgs: [ epkgs.avy ];
  elispFile = ./avy.el;
}
