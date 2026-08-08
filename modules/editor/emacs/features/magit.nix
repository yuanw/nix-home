{ ... }:

{
  epkgs = epkgs: [
    epkgs.magit
    epkgs.forge
  ];
  elispFile = ./magit.el;
}
