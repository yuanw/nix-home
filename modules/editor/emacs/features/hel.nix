{ ... }:

{
  epkgs = epkgs: [
    epkgs.hel
    epkgs.hel-leader
    epkgs.hel-collection
    epkgs.consult
    epkgs.embark-consult
    epkgs.which-key
  ];

  elispFile = ./hel.el;
}
