{ lib, options, ... }:

with lib;

let
  mkOpt' =
    type: default: description:
    mkOption { inherit type default description; };
  mkOptStr =
    value:
    mkOption {
      type = with types; uniq str;
      default = value;
    };

  # copied from https://github.com/cmacrae/config
  mailAddr = name: domain: "${name}@${domain}";
in
{
  # https://github.com/ahmedelgabri/dotfiles/blob/16bc31166025ed450654ae5be08e840525d4c02f/nix/modules/shared/settings.nix#L38
  # https://github.com/hlissner/dotfiles/blob/089f1a9da9018df9e5fc200c2d7bef70f4546026/modules/options.nix#L39
  # https://codeberg.org/adamcstephens/profile-parts/src/branch/main/parts/nixos.nix
  #  Use `lib.mkForce value` or `lib.mkDefault value` to change the priority on any of these definitions.
  options = with types; {
    my = {
      system = lib.mkOption {
        type = lib.types.enum [
          "aarch64-darwin"
          "aarch64-linux"
          "x86_64-darwin"
          "x86_64-linux"
        ];
        description = "The system used when defining the host";
        default = "aarch64-darwin";
      };

      username = mkOptStr "yuanwang";
      name = mkOptStr "Yuan Wang";
      email = mkOptStr (mailAddr "me" "yuanwang.ca");
      hostname = mkOptStr "yuan-mac";
      gpgKey = mkOptStr "BF2ADAA2A98F45E7";
      homeDirectory = mkOptStr "/Users/yuanwang";
      packages = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "The set of packages to appear in the user environment.";
      };
      monoFont = mkOptStr "PragmataPro Mono Liga Serif";
      font = mkOptStr "PragmataPro Liga";
      hm = {
        file = mkOpt' attrs { } "Files to place directly in $HOME";
        cacheHome = mkOpt' path "${home}/.cache" "Absolute path to directory holding application caches.";
        configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
        configHome =
          mkOpt' path "${home}/.config"
            "Absolute path to directory holding application configurations.";
        dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
        dataHome =
          mkOpt' path "${home}/.local/share"
            "Absolute path to directory holding application data.";
        stateHome =
          mkOpt' path "${home}/.local/state"
            "Absolute path to directory holding application states.";
      };
    };
  };
  # config = {
  #      my = {
  #   username = "yuanwang";
  #   name = "Yuan Wang";
  #   email = "yuan.wang@workiva.com";
  #   hostname = "wf17084";
  #   gpgKey = "19AD3F6B1A5BF3BF";
  #   homeDirectory = "/Users/yuanwang";
  # };
  # };
}
