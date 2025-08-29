{
  inputs,
  config,
  pkgs,
  ...
}:
let
  nixCustomConf = pkgs.writeText "nix.custom.conf" ''
        lazy-trees = true
    substituters = https://cache.nixos.org https://nix-community.cachix.org https://yuanw-nix-home-macos.cachix.org https://cachix.org/api/v1/cache/yuanwang-wf https://cachix.org/api/v1/cache/devenv https://cache.garnix.io https://cache.iog.io https://cache.zw3rk.com https://cache.nixos.org/
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80= devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw= yuanw-nix-home-macos.cachix.org-1:6sDjrV0jQY6kRgXjXe0feuDtsxnoGDnkgvXuKma5JcQ= cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
    trusted-substituters = https://cache.nixos.org https://nix-community.cachix.org https://yuanw-nix-home-macos.cachix.org https://cache.garnix.io https://cache.iog.io
  '';
in
{

  imports = [
    inputs.self.nixosModules.common
    inputs.self.nixosModules.darwin
  ];
  # determinate system
  nix.enable = false;
  my = {
    username = "yuanw";
    name = "Yuan Wang";
    hostname = "mist";
    workspaceDirectory = "workspaces";
    homeDirectory = "/Users/yuanw";
  };

  environment.etc."nix/nix.custom.conf".source = nixCustomConf;
  environment.systemPath = [
    "/opt/homebrew/bin"
    "/opt/homebrew/sbin"
  ];
  home-manager.users.${config.my.username}.programs = {
    go = {
      enable = true;
      goPath = "go";
    };
    git = {
      extraConfig = {
        github.user = "yuanw";
      };
    };
  };
  modules = {
    # common = {
    #   enable = true;
    #   supportLocalVirtualBuilder = true;
    # };
    ##ai.enable = true;
    secrets.agenix = {
      enable = true;
    };
    brew = {
      enable = true;
      casks = [
        "1password"
        "betterdisplay"
        "racket"
        "protonvpn"
        "vlc"
        "mouseless@preview"

      ];
      masApps = {
        "Fresh Eyes" = 6480411697;
        "Keystroke Pro" = 1572206224;
      };
      # taps = [ "homebrew/core" "homebrew/cask" ];
    };
    browsers.firefox = {
      enable = true;
    };
    editors.emacs = {
      enable = true;
      enableLatex = false;
      enableService = true;
      # enableAider = true;
      # enableCopilot = true;
      lspStyle = "lsp-bridge";
    };
    # health.enable = true;
    typing.enable = true;
    #jellyfin.enable = true;
    dev = {
      #agda.enable = true;
      #ask.enable = true;
      scheme.enable = true;
      lean.enable = true;
      #racket.enable = false;
      haskell.enable = false;
      #idris2.enable = true;
      python.enable = true;
      #zig.enable = false;
    };

    tmux = {
      enable = true;
      mainWorkspaceDir = "$HOME/workspaces";
    };
    terminal = {
      enable = true;
    };
    wm = {
      yabai.enable = true;
      yabai.enableJankyborders = true;
    };
  };
}
