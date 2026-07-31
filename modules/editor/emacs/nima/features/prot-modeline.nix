{ ... }:

{
  epkgs = epkgs: [
    (epkgs.trivialBuild {
      pname = "prot-common";
      version = "0.0.1";
      src = ../../packages/prot-common.el;
    })
    (epkgs.trivialBuild {
      pname = "prot-modeline";
      version = "0.0.1";
      src = ../../packages/prot-modeline.el;
      packageRequires = [
        (epkgs.trivialBuild {
          pname = "prot-common";
          version = "0.0.1";
          src = ../../packages/prot-common.el;
        })
      ];
    })
  ];

  elispFile = ../../configs/prot-modeline.el;
}
