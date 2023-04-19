{ config, lib, pkgs, home-manager, options, ... }:

with lib;

let
  mkOptStr = value:
    mkOption {
      type = with types; uniq str;
      default = value;
    };

  # copied from https://github.com/cmacrae/config
  mailAddr = name: domain: "${name}@${domain}";
in {
  # https://github.com/ahmedelgabri/dotfiles/blob/16bc31166025ed450654ae5be08e840525d4c02f/nix/modules/shared/settings.nix#L38
# https://github.com/hlissner/dotfiles/blob/089f1a9da9018df9e5fc200c2d7bef70f4546026/modules/options.nix#L39
  options = with types; {
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
}
