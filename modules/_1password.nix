# 1Password: the desktop app (pkgs._1password-gui) and the op CLI (pkgs._1password-cli).
#
# Both package sets already ship modules for the platforms this flake builds --
# nix-darwin/modules/programs/_1password{,-gui}.nix and
# nixpkgs/nixos/modules/programs/_1password{,-gui}.nix -- and they spell their
# options identically (programs._1password, programs._1password-gui), so this
# file is nothing but a switch that turns them on together and gives the host
# one knob instead of two.
#
# What the upstream modules do on their own, and why nothing here repeats it:
# the darwin one copies the bundle to /Applications/1Password.app and installs
# the CLI at /usr/local/bin/op -- the path the app's "integrate with CLI"
# setting looks for, and a directory already on PATH, so op needs no PATH
# wiring from us. That copy lives in /Applications proper, so a host must not
# also carry the same bundle as a cask: two LaunchServices registrations of one
# bundle is how the CLI integration and the auto-updater start fighting. Hence
# hosts/mist.nix dropped environment.casks."1password" when it enabled this.
#
# NixOS-side extras stay with the upstream module: if a desktop host ever wants
# polkit policy owners for the browser extension, it sets
# programs._1password-gui.polkitPolicyOwners itself (hosts/asche/configuration.nix
# does exactly that) -- this module deliberately does not forward such options,
# because the darwin module does not declare them and assigning to an undeclared
# option fails even under a false condition.
#
# Both packages are unfree and fetched from 1Password's own CDN; the
# allowUnfree in lib/mk-pkgs.nix is what lets them into the package set.
#
# Usage:
#   modules._1password.enable = true;                       # app + CLI
#   modules._1password = { enable = true; cli = false; };   # app only
{
  config,
  lib,
  ...
}:

with lib;
let
  cfg = config.modules._1password;
in
{
  options.modules._1password = {
    enable = mkEnableOption "1Password";

    gui = (mkEnableOption "the 1Password desktop application") // {
      default = true;
    };

    cli = (mkEnableOption "the 1Password CLI (op)") // {
      default = true;
    };
  };

  config = mkIf cfg.enable {
    programs._1password.enable = cfg.cli;
    programs._1password-gui.enable = cfg.gui;
  };
}
