{ config, lib, pkgs, isDarwin, ... }:
with lib;
let cfg = config.modules.common;
in {
  options = {
    modules.common = {
      enable = mkEnableOption "common";
      supportLocalVirtualBuilder = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    nix = {
      # configureBuildUsers = true;
      settings = {
        trusted-users = [ "root" config.my.username ];
        substituters = [
          "https://cache.nixos.org"
          "https://nix-community.cachix.org"
          "https://yuanw-nix-home-macos.cachix.org"
          "https://cachix.org/api/v1/cache/yuanwang-wf"
          "https://cachix.org/api/v1/cache/devenv"
        ];
        trusted-substituters = [
          "https://cache.nixos.org"
          "https://nix-community.cachix.org"
          "https://yuanw-nix-home-macos.cachix.org"
        ];
        trusted-public-keys = [
          "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "yuanwang-wf.cachix.org-1:P/RZ5Iuuuv2MYCNCnAsLfPGmgKMKeTwPaJclkrcwx80="
          "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
          "yuanw-nix-home-macos.cachix.org-1:6sDjrV0jQY6kRgXjXe0feuDtsxnoGDnkgvXuKma5JcQ="
        ];
        # https://github.com/NixOS/nix/issues/7273
        # auto-optimise-store = true;
        max-jobs = 4;
      };
      # Avoid unwanted garbage collection when using nix-direnv
      extraOptions = ''
        experimental-features = nix-command flakes repl-flake
        keep-outputs          = true
        keep-derivations      = true
        fallback              = true
        extra-trusted-users   = ${config.my.username}
      '' + (if cfg.supportLocalVirtualBuilder then ''
          # Not strictly necessary, but this will reduce your disk utilization
        builders-use-substitutes = true
        builders = ssh-ng://builder@localhost x86_64-linux /etc/nix/builder_ed25519 4 - - - c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=

      '' else
        "");
      # trustedBinaryCaches = config.nix.binaryCaches;
      gc = {
        automatic = true;
        # interval = { Hour = 24 * 7; };
      };
    };
    nixpkgs = {
      config = {
        allowUnfree = true;
        allowBroken = false;
        allowUnsupportedSystem = true;
      };
    };
  };
}
