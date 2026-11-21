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
          # No basic_auth here. Prowlarr has its own account now (Settings -> General
          # -> Authentication Method "Forms (Login Page)"), so the credential lives in
          # the app that actually enforces it and the user is prompted once, not twice.
          # Keep this vhost free of a second gate: it would only add a prompt in front
          # of a login screen, and Prowlarr's own auth is what protects indexers and
          # download clients. Belt-and-braces option if the app login is ever removed:
          # put basic_auth back, using the bcrypt verifier of pass:prowlarr/caddy-password.
          reverse_proxy localhost:9696
        '';
      };

      # qBittorrent's WebUI. The daemon lives in the `seeder` network namespace
      # (modules/mullvad.nix), so it is NOT reachable on localhost — this address is the
      # veth peer inside that namespace, which the host can route to via sd-h. Using
      # localhost here would proxy to nothing: the namespace's loopback is its own, and
      # that dead localhost is exactly what proved qbittorrent had really moved.
      #
      # No basic_auth, same reasoning as prowlarr.yuanw.me: the WebUI has its own login
      # (user yuan, password in pass:misfit/qbittorrent/password; its PBKDF2 verifier is
      # the confidentiality boundary in modules/arr.nix, which is why that file is
      # private). A second prompt in front of a login screen buys nothing.
      #
      # The danger specific to qBittorrent: WebUI\auth_ip_whitelist lets listed source
      # addresses log in with no credentials at all, and behind a reverse proxy every
      # browser arrives as Caddy. Default whitelist is 127.0.0.1/32, ::1/128 and
      # 192.168.0.0/16, and Caddy connects from the host-side veth 10.240.0.1 — not in
      # that list, so the login is enforced. If that setting is ever widened, this vhost
      # becomes an unauthenticated remote control over the torrent daemon, and an
      # attacker could point add-torrent save paths anywhere the qbittorrent user writes.
      # Verified by test, not by reading the default: unauthenticated
      # GET /api/v2/app/version through this vhost must answer 403.
      #
      # CSRF: qBittorrent checks Origin/Referer against Host, and Caddy forwards Host
      # unchanged, so a browser at https://qbittorrent.yuanw.me matches. Scripted API
      # calls must send Referer/Origin for http://10.240.0.2:8080/ instead.
      # qBittorrent's WebUI. The daemon lives in the `seeder` network namespace
      # (modules/mullvad.nix), so it is NOT reachable on localhost — this address is the
      # veth peer inside that namespace, which the host routes to via sd-h. Proxying to
      # localhost here would reach nothing: loopback inside the namespace is its own, and
      # that dead localhost is exactly what proved qbittorrent had really moved.
      #
      # No basic_auth, same reasoning as prowlarr.yuanw.me: the WebUI has its own login
      # (user yuan, password in pass:misfit/qbittorrent/password; the PBKDF2 verifier in
      # modules/arr.nix is the confidentiality boundary, which is why that file is
      # private). A second prompt in front of a login screen buys nothing.
      #
      # The risk specific to qBittorrent: WebUI\auth_ip_whitelist lets listed source
      # addresses in with no credentials at all, and behind a reverse proxy every browser
      # arrives as Caddy. The default list is 127.0.0.1/32, ::1/128 and 192.168.0.0/16;
      # Caddy connects from the host-side veth 10.240.0.1, which is not on it, so the
      # login is enforced. Widen that setting and this vhost becomes an unauthenticated
      # remote control over the torrent daemon — including choosing where add-torrent
      # writes. So it is verified by test, not by trusting a default: unauthenticated
      # GET /api/v2/app/version through this vhost must answer 403.
      #
      # CSRF: qBittorrent compares Origin/Referer against Host, and Caddy forwards Host
      # unchanged, so a browser at this hostname matches. Scripted API calls are the ones
      # that must send Referer/Origin for http://10.240.0.2:8080/ by hand.
      "qbittorrent.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy 10.240.0.2:8080
        '';
      };

      # The arr stack's two UIs that have a login worth publishing.  Same shape as
      # prowlarr.yuanw.me and the same three reasons, so they are stated once:
      #
      #   * the upstream binds to 127.0.0.1 (services.*.settings.server.bindAddress
      #     in hosts/misfit/configuration.nix) and no firewall rule exists for
      #     7878/8989, so this vhost is the only way in from anywhere else;
      #   * no basic_auth — each app has its own login (username yuan, password in
      #     pass:radarr/admin-password / pass:sonarr/admin-password, both declared
      #     through agenix rather than clicked in the UI), and a prompt in front of
      #     a login screen buys nothing;
      #   * the apps are told they are proxied (settings.server.isProxied) or their
      #     CSRF check rejects the login POST and their logs call every browser
      #     127.0.0.1.
      "radarr.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy localhost:7878
        '';
      };

      "sonarr.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy localhost:8989
        '';
      };

      # No bazarr.yuanw.me, deliberately.  Bazarr's local login is opt-in and its
      # config says `auth: {type: null, username: '', password: ''}` as shipped
      # (measured in /var/lib/bazarr/config/config.yaml), so a vhost here would
      # publish a subtitle daemon whose settings page — including the Radarr and
      # Sonarr connection fields — answers to anyone who can resolve the name.
      # Its own UI is rarely needed now that its configuration is declarative
      # (services.bazarr above); when it is, use a tunnel and no new listener:
      #   ssh -N -L 6767:127.0.0.1:6767 misfit    ->  http://localhost:6767

      "agh.yuanw.me" = {
        useACMEHost = "yuanw.me";
        extraConfig = ''
          reverse_proxy http://192.168.1.1:3000
        '';
      };
    };
  };
}
