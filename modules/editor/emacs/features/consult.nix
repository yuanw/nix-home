{ ... }:

{
  epkgs = epkgs: [
    epkgs.consult
    epkgs.consult-project-extra
  ];
  elispFile = ./consult.el;
}
