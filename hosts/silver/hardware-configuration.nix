# Intel MacBookPro11,2 (15" Mid 2014) hardware configuration.
#
# Placeholder: regenerate on the target after install with
#   nixos-generate-config --show-hardware-config
# The fileSystems/swap entries are supplied by disko (../disk-config.nix), so
# only the initrd/module bits live here.
{ lib, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "usb_storage"
    "usbhid"
    "sd_mod"
    "sdhci_pci"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
