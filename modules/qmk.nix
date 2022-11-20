{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.qmk;
in {
  options.modules.qmk = { enable = mkEnableOption "qmk"; };

  config = mkIf cfg.enable {
    home-manager.users.${config.my.username} = {
      home.packages = [ pkgs.qmk pkgs.libusb1 pkgs.hid-listen ];
    };
    services.udev.packages = [ pkgs.qmk-udev-rules ];

  };
}
