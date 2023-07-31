{ ... }:
{
  flake.nixosModules = {
    common =
      ./common.nix;
    darwin.imports = [
      ./brew.nix
      ./wm/yabai.nix
    ];

    # desktop = ./desktop.nix;
    # gnome = ./gnome.nix;
    # gotosocial = ./gotosocial.nix;
    # nix-remote-builders = ./nix-remote-builders.nix;
    # server = ./server.nix;
  };
}
