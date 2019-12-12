self: super: {

  lorri =
    let src = super.fetchFromGitHub {
          owner  = "target";
          repo   = "lorri";
          # rev    = "refs/master";
          rev    = "152a3ccdc3025dd12f0bdb69a260550f12114722";
          sha256 = "1915f027766kjfy650pxw9zzsk7ifxgn48dkr38wrlqrvlpxyv4s";
          # date = 2019-12-11T12:35:56+01:00;
          fetchSubmodules = true;
        }; in
      import src { inherit src; pkgs = self; };

}
