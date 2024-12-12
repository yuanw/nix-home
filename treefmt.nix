{
  projectRootFile = "flake.lock";
  programs.deadnix.enable = true;
  programs.nixpkgs-fmt.enable = true;
  programs.nixpkgs-fmt.excludes = [ "/modules/private" "/hosts/wk01174.nix" ];

  programs.ormolu.enable = true;
  programs.cabal-fmt.enable = true;
  programs.hlint.enable = true;
  programs.shellcheck.enable = true;
  programs.shfmt.enable = true;
}
