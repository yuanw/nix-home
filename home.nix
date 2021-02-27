{ pkgs, ... }:

{
  home.username = "yuanwang";
  home.homeDirectory = "$HOME";
  home.stateVersion = "20.09";

  home.packages = [ pkgs.htop pkgs.fortune ];
  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };
}
