{ config, lib, pkgs, ... }:

with pkgs.stdenv;
with lib; {
  imports = [ ./hardware-configuration.nix ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    enableCryptodisk = true;
    device = "nodev";
  };
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # luks
  boot.initrd.luks.devices.cryptboot = {
    preLVM = true;
    device = "/dev/disk/by-uuid/23aa6e7d-55ae-4f77-8994-bdbdcc8680a0";
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  #networking.interfaces.wlp0s20f0u4u4.useDHCP = true;

}
