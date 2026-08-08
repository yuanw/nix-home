{ ... }:

{
  epkgs = epkgs: [
    epkgs.embark
    epkgs.embark-consult
  ];
  elispFile = ./embark.el;
}
