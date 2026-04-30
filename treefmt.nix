{
  projectRootFile = "flake.lock";
  settings.global.excludes = [ "modules/private/get-snapshot-version.sh" ];
  programs.deadnix.enable = true;
  programs.nixfmt.enable = true;
  settings.formatter.nixfmt = {
    excludes = [ "modules/private/*" ];
  };
  #  programs.ormolu.enable = true;
  programs.cabal-fmt.enable = true;
  # programs.hlint.enable = true;
  programs.shellcheck.enable = true;
  settings.formatter.shellcheck = {
    excludes = [
      "modules/private/*"
      "modules/private/get-snapshot-version.sh"
    ];
  };
  programs.shfmt.enable = true;
  settings.formatter.shfmt = {
    excludes = [
      "modules/private/*"
      "modules/private/get-snapshot-version.sh"
    ];
  };
}
