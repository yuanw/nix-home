{
  projectRootFile = "flake.lock";
  settings.global.excludes = [ ];
  programs.deadnix.enable = true;
  programs.nixfmt.enable = true;
  settings.formatter.nixfmt = {
    excludes = [ ];
  };
  #  programs.ormolu.enable = true;
  programs.cabal-fmt.enable = true;
  # programs.hlint.enable = true;
  programs.shellcheck.enable = true;
  settings.formatter.shellcheck = {
    excludes = [ ];
  };
  programs.shfmt.enable = true;
  settings.formatter.shfmt = {
    excludes = [ ];
  };
}
