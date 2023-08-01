{ ... }:
{
  flake.nixosModules = {
    common.imports  = [
      ./common.nix
      ./browsers/firefox.nix
    ];
    darwin.imports = [
      ./brew.nix
      ./wm/yabai.nix
      ../macintosh.nix
    ];

    # desktop = ./desktop.nix;
    # gnome = ./gnome.nix;
    # gotosocial = ./gotosocial.nix;
    # nix-remote-builders = ./nix-remote-builders.nix;
    # server = ./server.nix;
  };
}
