# SSH into mist from the phone (Tailscale)

Goal: `ssh yuan@mist` from iOS. Tailscale carries the tunnel, macOS `sshd`
carries the shell. Checked against the live machine on 2026-10-07.

## What already works

- `tailscaled` 1.102.5 runs as a LaunchDaemon (`services.tailscale.enable` in
  `hosts/mist.nix` → nix-darwin `launchd.daemons.tailscaled`). Tunnel state was
  `Running`, already authenticated (`AuthURL` empty), so no `tailscale up`
  after a reboot.
- Tailnet `tail2e1667.ts.net`: mist = `100.79.218.109` / `mist.tail2e1667.ts.net`.
- The phone already has a node: `100.73.232.95 ios yuanw@ iOS` — **offline, last
  seen 5d ago**. That is the real gap; nothing on the Mac was missing.
- `sshd` (OpenSSH_10.3) answers on :22 on both the tailnet and the LAN
  (192.168.1.189) addresses, offering `publickey,password,keyboard-interactive`.
- Tailscale SSH is off (`RunSSH: false`) and stays off — see "Skipped".

## 1. Put the phone back on the tailnet

1. [admin console](https://login.tailscale.com/admin/machines): is `ios` still
   there and **authorized**? If it was removed, or the app was reinstalled, it
   needs a fresh login with the `yuanw@` account (and `sudo tailscale up` on
   any machine that has to re-join).
2. Otherwise just turn the VPN back on: Settings → General → VPN & Device
   Management → VPN → Tailscale → On (or open the Tailscale app).
3. One tunnel per device: if you use Mullvad/Proton on the phone, Tailscale has
   to win for this to work. Same on the Mac.

Check from mist:

    tailscale status | grep ios          # want "active", not "offline"
    tailscale ping 100.73.232.95         # tailnet path + ACLs

`ping` timing out while the node shows online = ACL problem. Default ACLs allow
everything between members; a custom policy needs a `ios → mist:22` line (no
`ssh` section — we are not using Tailscale SSH).

## 2. SSH client and key on the phone

Client: Termius (free is enough), Blink Shell (paid), or Shellfish. Host `mist`
(MagicDNS is on, `CorpDNS: true`, so the short name resolves while the VPN is
up) or `100.79.218.109` if names misbehave, port 22, user `yuan`, key auth.

Key: generated on the phone and kept in the Secure Enclave / iCloud Keychain,
or kept in 1Password as a *Private Key* and imported into Termius.

    SHA256:wkw3PZlh0idk+1plO2C/hRvksepONEXf5S+57fbm2z8  (phone)

## 3. sshd on mist

`hosts/mist.nix` now carries the key and refuses passwords:

```nix
  services.openssh = {
    enable = true;
    extraConfig = ''
      PasswordAuthentication no
      MaxAuthTries 3
    '';
  };

  users.users.${config.my.username}.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAA... phone"
  ];
```

- `authorizedKeys.keys` feeds `/etc/ssh/nix_authorized_keys.d/yuan`, which the
  stock `AuthorizedKeysCommand /bin/cat /etc/ssh/nix_authorized_keys.d/%u`
  already reads — `~/.ssh/authorized_keys` stays untouched.
- That file is committed in a **public** repo. A public key is not a secret, but
  this one is a loaded door key: if the repo ever goes public, move the key to
  the private repo and point `authorizedKeys.keyFiles` at it.
- `PasswordAuthentication no` because a password you can type on glass is a
  password worth phishing, and it buys nothing once the phone has a key.
- No `ListenAddress`/`AddressFamily` to keep SSH off the LAN: launchd owns the
  :22 socket (`SocketServiceName: ssh` in
  `/System/Library/LaunchDaemons/ssh.plist`) and hands it to sshd, so a
  `ListenAddress` would not narrow the listener. Port 22 stays reachable from
  the LAN and from any authorized tailnet node. The lever that *would* work is
  an address match — `AllowUsers yuan@100.64.0.0/10` plus the tailnet v6 range,
  since `AllowUsers`/`DenyUsers` are default-deny as soon as one is listed —
  but with the tunnel down the phone has no route to mist anyway, so it only
  buys protection from your own LAN. Add it if that ever matters.

## 4. After `just switch`

sshd keeps running with its old config: `services.openssh.enable` only touches
the Remote Login toggle when it is *Off*, and a running sshd does not re-read
`/etc/ssh/sshd_config.d/`. Restart it:

```
sudo launchctl bootout system/com.openssh.sshd
sudo launchctl bootstrap system /System/Library/LaunchDaemons/ssh.plist
```

## 5. Verify

    # on mist: password auth gone
    ssh -o PreferredAuthentications=none -o StrictHostKeyChecking=no yuan@127.0.0.1 exit
    # want: Permission denied (publickey)   -- not (publickey,password,...)

    tailscale ping 100.73.232.95           # want: reply from the phone

    # on the phone
    ssh -v yuan@mist                       # then `tmux` / `tat`

To smoke-test the tailnet path without the phone, `nc -vz mist.tail2e1667.ts.net 22`
from dgx-spark or misfit — both are in the tailnet.

## Skipped on purpose

- **Tailscale SSH.** Needs the App Store `Tailscale.app` and a macOS account
  whose username *and* password match your tailnet login. This box runs the
  nixpkgs Go daemon (`pkgs.tailscale`), which has no inbound SSH server, so
  plain `sshd` on 22 is fewer layers, not more. Keep `RunSSH = false`.
- **`services.tailscale.overrideLocalDns`.** Default `false`, and there is an
  assertion if you flip it without also setting `networking.dns`. The
  `openat ts.net: path escapes from parent` health-check line in
  `tailscale status` is Tailscale's DNS writer tripping over the
  nix-darwin-managed `/etc/resolver/ts.net` symlink; name resolution still
  works (both `tail2e1667.ts.net` and `ts.net` resolvers exist). Cosmetic —
  leave it.
- **`tailscale serve` / SFTP-over-Tailscale.** `scp`/`sftp` over 22 does the
  same job with one less daemon.
