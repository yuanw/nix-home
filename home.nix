{ pkgs, ... }:

{
  home.username = "yuanwang";
  home.homeDirectory = "/Users/yuanwang";
  home.stateVersion = "20.09";

  home.packages = [ pkgs.htop pkgs.fortune ];
  home.file = {
    ".ghci".text = ''
      :set prompt "λ> "
    '';
  };
}
