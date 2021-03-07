{ config, lib, ... }:
with lib;
let cfg = config.dotfiles;
in {
  options = {
    dotfiles = {
      username = mkOption { type = types.str; };
      name = mkOption { type = types.str; };
      email = mkOption { type = types.str; };
      hostname = mkOption { type = types.str; };
      gpgKey = mkOption {
        type = types.str;
        default = "";
      };
    };
  };

  config = { };

}
