{
  projectRootFile = "flake.lock";
  programs.deadnix.enable = true;
  programs.nixfmt.enable = true;
  #programs.nixfmt.excludes = [ "/modules/private/*" "/hosts/wk01174.nix" ];
  settings.formatter.nixfmt = {
    excludes = [ "work.nix" ];
  };
  programs.ormolu.enable = true;
  programs.cabal-fmt.enable = true;
  programs.hlint.enable = true;
  programs.shellcheck.enable = true;
  programs.shfmt.enable = true;
}
