# Thank you cprussin - https://github.com/cprussin/dotfiles
{ config, lib, ... }:
let cfg = config.primary-user;
in {
  options.primary-user.name = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = "The name of the primary user account.";
  };

  options.primary-user.userName = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = "The user name of the primary user account, mainly for git";
  };

  options.primary-user.email = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = "The email of the primary user account, mainly for git";
  };

  imports = [
    (lib.mkAliasOptionModule [ "primary-user" "home-manager" ] [
      "home-manager"
      "users"
      cfg.name
    ])
    (lib.mkAliasOptionModule [ "primary-user" "home" ] [
      "users"
      "users"
      cfg.name
      "home"
    ])
    (lib.mkAliasOptionModule [ "primary-user" "shell" ] [
      "users"
      "users"
      cfg.name
      "shell"
    ])
    (lib.mkAliasOptionModule [ "primary-user" "extraGroups" ] [
      "users"
      "users"
      cfg.name
      "extraGroups"
    ])
    (lib.mkAliasOptionModule [ "primary-user" "uid" ] [
      "users"
      "users"
      cfg.name
      "uid"
    ])
    (lib.mkAliasOptionModule [ "primary-user" "isNormalUser" ] [
      "users"
      "users"
      cfg.name
      "isNormalUser"
    ])
    (lib.mkAliasOptionModule [ "primary-user" "hashedPassword" ] [
      "users"
      "users"
      cfg.name
      "hashedPassword"
    ])
  ];

  config = lib.mkIf (cfg.name != null) {
    primary-user = { uid = lib.mkDefault 1000; };
    nix.trustedUsers = [ "root" cfg.name ];
  };
}
