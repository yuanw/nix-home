{
  projectRootFile = "flake.lock";
  programs.deadnix.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.ormolu.enable = true;
  programs.cabal-fmt.enable = true;
  programs.hlint.enable = true;
  programs.shfmt.enable = true;
}
