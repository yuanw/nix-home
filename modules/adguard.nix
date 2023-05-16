{ modulesPath, pkgs, ... }:

{

  imports = [ "${modulesPath}/virtualisation/amazon-image.nix" ];

  documentation.enable = false;
  environment.systemPackages = [ pkgs.bind ];
  networking.firewall = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPortRanges = [{
      from = 53;
      to = 53;
    }];
  };

    system.activationScripts.adguard-passwords = ''
        if    [ -e "$STATE_DIRECTORY/AdGuardHome.yaml" ] \
           && [ "${toString cfg.mutableSettings}" = "1" ]; then
          # Writing directly to AdGuardHome.yaml results in empty file
          ${pkgs.yaml-merge}/bin/yaml-merge "$STATE_DIRECTORY/AdGuardHome.yaml" "${configFile}" > "$STATE_DIRECTORY/AdGuardHome.yaml.tmp"
          mv "$STATE_DIRECTORY/AdGuardHome.yaml.tmp" "$STATE_DIRECTORY/AdGuardHome.yaml"
        else
          cp --force "${configFile}" "$STATE_DIRECTORY/AdGuardHome.yaml"
          chmod 600 "$STATE_DIRECTORY/AdGuardHome.yaml"
        fi
        '';

  services.adguardhome = {
    enable = true;
    openFirewall = true;
    mutableSettings = true;
    # corresponds to /var/lib/AdGuardHome/AdGuardHome.yaml
    settings = {
      block_auth_min = 10;
      debug_pprof = true;
      dns = {
        bind_hosts = [ "0.0.0.0" ];
        # query logging
        querylog_enabled = true;
        querylog_file_enabled = true;
        querylog_interval = "24h";
        querylog_size_memory = 1000; # entries
        anonymize_client_ip = false; # for now

        # adguard
        protection_enabled = true;
        blocking_mode = "default"; # NXDOMAIN
        filtering_enabled = true;

        # upstream DNS
        upstream_dns = [
          "8.8.8.8" # google
          "8.8.8.4" # google
        ];
        # if upstream has any hostnames
        bootstrap_dns = [ "192.168.1.1" ]; # ask the gateway
        # caching
        cache_size = 536870912; # 512 MB
        cache_ttl_min = 1800; # 30 min
        cache_optimistic = true; # return stale and then refresh
      };
    };
  };

  system.stateVersion = "22.11";

}
