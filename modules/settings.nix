{ config, lib, pkgs, home-manager, options, ... }:

with lib;

let
  mkOptStr = value:
    mkOption {
      type = with types; uniq str;
      default = value;
    };

  mailAddr = name: domain: "${name}@${domain}";
in {

  options = with types; {
    my = {
      username = mkOptStr "yuanwang";
      name = mkOptStr "Yuan Wang";
      email = mkOptStr (mailAddr "me" "yuanwang.ca");
      hostname = mkOptStr "yuan-mac";
      gpgKey = mkOptStr "BF2ADAA2A98F45E7";
      homeDirectory = mkOptStr "/Users/yuanwang";

    };
  };
}
