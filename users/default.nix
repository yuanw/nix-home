{ lib, ... }:
let
  mySubmodule = lib.types.submodule {
    options = {
      username = lib.mkOption {
        type = lib.types.str;
      };
      email = lib.mkOption {
        type = lib.types.str;
      };
      gpgKey = lib.mkOption {
        type = lib.types.str;
        description = ''
          GPP key
        '';
      };
      homeDirectory = lib.mkOption {
        type = lib.types.str;
        description = ''
        '';
      };
      font = lib.mkOption {
        type = lib.types.str;
        description = ''
        '';
      };
      hstname = lib.mkOption {
        type = lib.types.str;
        description = ''
        '';
      };
      name = lib.mkOption {
        type = lib.types.str;
        description = ''
        '';
      };

      sshKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = ''
          SSH public keys
        '';
      };
    };
  };
in
{
  options = {
    my = lib.mkOption {
      type = mySubmodule;
    };
  };
  config = {
    my = import ./config.nix;
  };
}
