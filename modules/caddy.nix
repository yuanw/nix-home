{ ... }:
{

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];

  services.caddy = {
    enable = true;
    globalConfig = ''
      auto_https off
    '';
    virtualHosts = {
      "http://*.yuanw.me" = {
        extraConfig = ''
          redir https://{host}{uri}
        '';
      };
      "ha.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy localhost:8123
        '';
      };

      "jellyfin.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy localhost:8096
        '';
      };

      # LAN-only names. The board's dnsmasq sends *every* *.yuanw.me to misfit
      # (hosts/mono-gateway/default.nix -- the override moved here from the e8450 at
      # the swap), so these just need a vhost here; no DNS change.
      # If that ever changes, gate these: a router admin UI, a Cockpit root
      # shell, and an unauthenticated inference API must not be internet-facing.
      "openwrt.yuanw.me" = {
        useACMEHost = "yuanw.me";
        # The e8450, which is the AP now: it gave up .1 to the Mono board at the
        # swap and answers LuCI on .2. Hardcoding .1 here would proxy to the
        # gateway's sshd instead, which is both wrong and silently useless.
        extraConfig = ''
          reverse_proxy http://192.168.1.2
        '';
      };

      # Upstreams here must resolve via plain DNS on misfit: mDNS `.local`
      # names do not (see the targetHost note in hosts/misfit/default.nix),
      # so use the router's dnsmasq/DHCP `.lan` names.
      "cockpit.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy http://dgx-spark.lan:9090
        '';
      };

      # TensorFold Zig (OpenAI-compatible API, no auth) on dgx-spark:8888.
      # Same LAN-only caveat as the two above: an unauthenticated inference
      # endpoint must not become internet-reachable.
      "tensorfold.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy http://dgx-spark.lan:8888
        '';
      };

      # HealthSave Observatory API: the HealthSave iOS app's Server Sync URL is
      # https://healthsave.yuanw.me (see the note in modules/caddy.nix's other
      # LAN-only entries — this one *is* internet-reachable, gated only by the
      # API_KEY in secrets/healthsave-env.age). /metrics is unauthenticated by
      # design upstream, so it stays on the box.
      "healthsave.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          @metrics path /metrics /metrics/*
          respond @metrics "forbidden" 403
          reverse_proxy localhost:8000
        '';
      };

      # AdGuard Home's UI. The board binds 3000 on br-lan (hosts/mono-gateway/default.nix)
      # and gates it with auth.users, so this is the only path where the login
      # travels over TLS -- the direct http://192.168.1.1:3000 path still exists
      # for any LAN host. No DNS change: the board's =.yuanw.me= wildcard already
      # lands any name on misfit.
      # Prowlarr's UI. Same LAN-only caveat as the entries above — the servarr UI
      # has no authentication of its own, so this is only as private as DNS makes
      # it. That is survivable only because services.prowlarr.settings.server.bindAddress
      # is "localhost" (hosts/misfit/configuration.nix): if that ever flips back to
      # "*", 9696 becomes directly reachable and unauthenticated.
      # Prowlarr's own UI needs no path rewriting, but isProxied = true is required
      # or its logs/CSRF treat every request as coming from 127.0.0.1.
      "prowlarr.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          # Caddy-side credential, because the servarr UI has no account of its own
          # that Nix can declare (users live in prowlarr.db, while settings.* only
          # writes config.xml). Prowlarr is bound to 127.0.0.1 and sees Caddy as its
          # only client, so it never prompts; this is the gate.
          #
          # bcrypt verifier from `pass show prowlarr/caddy-password`, hashed with the
          # box's own caddy binary (`caddy hash-password`). Uncrackable at 32 random
          # chars, so unlike the qBittorrent PBKDF2 hash it is safe to commit — that
          # one guards a human-chosen password and stays in agenix. Rotate with:
          #   pass generate -f prowlarr/caddy-password 32
          # and re-hash into this line.
          basic_auth {
            yuan $2a$14$kkI06jx9dIZPecxSuB5WfOSbOcwqDv.Ma4ewSvPgoeFWl3Q01xdEa
          }

          reverse_proxy localhost:9696
        '';
      };

      "agh.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy http://192.168.1.1:3000
        '';
      };
    };
  };
}
