{ config, lib, options, pkgs, ... }:
let

  cfg = config.modules.secrets.agenix;
  # settings = {
  #   block_auth_min = 10;
  #   debug_pprof = true;
  #   dns = {
  #     bind_hosts = [ "0.0.0.0" ];
  #     # query logging
  #     querylog_enabled = true;
  #     querylog_file_enabled = true;
  #     querylog_interval = "24h";
  #     querylog_size_memory = 1000; # entries
  #     anonymize_client_ip = false; # for now

  #     # adguard
  #     protection_enabled = true;
  #     blocking_mode = "default"; # NXDOMAIN
  #     filtering_enabled = true;

  #     # upstream DNS
  #     upstream_dns = [
  #       "8.8.8.8" # google
  #       "8.8.8.4" # google
  #     ];
  #     # if upstream has any hostnames
  #     bootstrap_dns = [ "192.168.1.1" ]; # ask the gateway
  #     # caching
  #     cache_size = 536870912; # 512 MB
  #     cache_ttl_min = 1800; # 30 min
  #     cache_optimistic = true; # return stale and then refresh

  #   };
  # };
  # user = "test";
  # configFile = pkgs.writeTextFile {
  #   name = "AdGuardHome.yaml";
  #   text = builtins.toJSON settings;
  # };
in
with lib;
with builtins; {

  options = {
    modules.secrets.agenix = {
      enable = mkEnableOption "agenix";

    };
  };
  config = mkIf cfg.enable (mkMerge [
    { environment.systemPackages = with pkgs; [ agenix rage ]; }

    (if (builtins.hasAttr "launchd" options) then {
      launchd.daemons.activate-agenix.serviceConfig = {
        StandardOutPath = "/tmp/agenix.out.log";
        StandardErrorPath = "/tmp/agenix.err.log";
      };
    } else
      {
        # systemd
      })

    {
      age = {
        #  secrets.secret1 = {
        #    file = ../secrets/secret1.age;
        # #   mode = "770";
        # #   owner = "yuanwang";
        # #   group = "admin";
        # };
        # secrets.adguard = {
        #   file = ../secrets/adguard.age;
        # mode = "770";
        # owner = "adguardhome-with-user";
        # group = "admin";
      };
      # does not work well with ec-2 instance
      # identityPaths = options.age.identityPaths.default
      #   ++ (filter pathExists [
      #   "${config.my.homeDirectory}/.ssh/id_ed25519"
      #   "${config.my.homeDirectory}/.ssh/id_rsa"
      # ]);

    };
    }

  ]);
}
