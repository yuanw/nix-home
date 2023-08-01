{ lib, ... }:
let
   mkOptStr = value:
    lib.mkOption {
      type = with lib.types; uniq str;
      default = value;
    };

  # copied from https://github.com/cmacrae/config
  mailAddr = name: domain: "${name}@${domain}";

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
        default = "PragmataPro";
      };
      hostname = lib.mkOption {
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
     my = {
      username = mkOptStr "yuanwang";
      name = mkOptStr "Yuan Wang";
      email = mkOptStr (mailAddr "me" "yuanwang.ca");
      hostname = mkOptStr "yuan-mac";
      gpgKey = mkOptStr "BF2ADAA2A98F45E7";
      homeDirectory = mkOptStr "/Users/yuanwang";
      font = mkOptStr "PragmataPro";
    };
  };
  # config = {
  #   my = import ./config.nix;
  # };
}
