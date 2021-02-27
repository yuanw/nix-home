{ pkgs, ... }:

{
  home.username = "yuanwang";
  home.homeDirectory = "/Users/yuanwang";
  home.stateVersion = "20.09";
  home.packages = (import ./modules/packages.nix { inherit pkgs; });
  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };
}
