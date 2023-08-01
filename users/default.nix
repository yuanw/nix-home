{ lib, ... }:
let
  mkOptStr = value:
    lib.mkOption {
      type = with lib.types; uniq str;
      default = value;
    };

  # copied from https://github.com/cmacrae/config
  mailAddr = name: domain: "${name}@${domain}";
in
{
  options.my = {
    username = mkOptStr "yuanwang";
    name = mkOptStr "Yuan Wang";
    email = mkOptStr (mailAddr "me" "yuanwang.ca");
    hostname = mkOptStr "yuan-mac";
    gpgKey = mkOptStr "BF2ADAA2A98F45E7";
    homeDirectory = mkOptStr "/Users/yuanwang";
    font = mkOptStr "PragmataPro";
  };
}
