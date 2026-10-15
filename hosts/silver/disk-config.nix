{ lib, ... }:

{
  disko.devices = {
    disk.main = {
      # MacBookPro11,2 internal PCIe AHCI SSD. VERIFY with `lsblk` from the
      # installer before running disko; `disko-install --disk main /dev/xxx`
      # overrides this anyway, so a wrong default cannot silently wipe the
      # wrong disk when the override is used.
      device = lib.mkDefault "/dev/sda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          esp = {
            size = "1G";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "umask=0077" ];
            };
          };
          root = {
            name = "root";
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
            };
          };
        };
      };
    };
  };
}
