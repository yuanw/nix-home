{ ... }:
{
  flake.nixosModules = {
    common.imports = [
      ./agenix.nix
      ./common.nix
      ./browsers/firefox.nix
      ./dev/agda.nix
      ./dev/dart.nix
      ./dev/haskell.nix
      ./dev/java.nix
      ./dev/julia.nix
      ./dev/node.nix
      ./dev/python.nix
      ./dev/zig.nix
      ./editor.nix
      ./helix.nix
      ./neovim
      ./health.nix
      ./settings.nix
      ./terminal
      ./typing
      ./workShell.nix
    ];
    darwin.imports = [
      ./brew.nix
      ./wm/yabai.nix
      ../macintosh.nix
      ({  ... }: {
        my = {
          username = "yuanwang";
          name = "Yuan Wang";
          email = "yuan.wang@workiva.com";
          hostname = "WK01174";
          gpgKey = "19AD3F6B1A5BF3BF";
          homeDirectory = "/Users/yuanwang";
        };
      })
    ];


    # desktop = ./desktop.nix;
    # gnome = ./gnome.nix;
    # gotosocial = ./gotosocial.nix;
    # nix-remote-builders = ./nix-remote-builders.nix;
    # server = ./server.nix;
  };
}
