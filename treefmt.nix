{
  projectRootFile = "flake.lock";
  programs.deadnix.enable = true;
  programs.nixfmt.enable = true;
  settings.formatter.nixfmt = {
    excludes = [ "modules/private/*" ];
  };
  programs.ormolu.enable = true;
  programs.cabal-fmt.enable = true;
  programs.hlint.enable = true;
  programs.shellcheck.enable = true;
  programs.shfmt.enable = true;
}
