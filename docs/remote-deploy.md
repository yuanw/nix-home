# Remote NixOS Deployment

This document covers how to deploy NixOS configurations from a build machine
(typically your local workstation or CI) to a remote NixOS host — without
plugging in a monitor or keyboard.

It covers the approaches used in this repo (`nixos-rebuild` with
`--target-host`, the `spark-deploy` just recipe, and `nixos-anywhere` for
initial provisioning) as well as general concepts and alternatives.

## Table of Contents

- [Why remote deploy?](#why-remote-deploy)
- [Prerequisites](#prerequisites)
- [Approach 1: `nixos-rebuild` with `--target-host`](#approach-1-nixos-rebuild-with---target-host)
- [Approach 2: Build on the remote host](#approach-2-build-on-the-remote-host)
- [Approach 3: `nixos-anywhere` (initial install)](#approach-3-nixos-anywhere-initial-install)
- [Approach 4: `deploy-rs` (multi-host orchestrator)](#approach-4-deploy-rs-multi-host-orchestrator)
- [Secrets with agenix on remote targets](#secrets-with-agenix-on-remote-targets)
- [This repo's workflow](#this-repos-workflow)
- [Troubleshooting](#troubleshooting)

## Why remote deploy?

- **Headless servers** — many NixOS machines (DGX Spark, VPS, Raspberry Pi)
  have no monitor or keyboard.
- **CI/CD** — push to Git, have CI build and deploy automatically.
- **Reproducibility** — the build runs against a known nixpkgs revision, not
  whatever the remote happens to have cached.
- **Speed** — build on a powerful machine, ship the binary closure to the
  target.

## Prerequisites

### SSH access

The deploying user needs SSH access to the target machine. The target must
be running an SSH server (`services.openssh.enable = true`).

```
ssh-keygen -t ed25519
ssh-copy-id yuanw@target-host
```

Verify access before attempting a deploy:

```
ssh yuanw@target-host "hostname"
```

### Sudo access without password

`nixos-rebuild` on the remote machine needs to run as root (or with passwordless
sudo) because it activates the new system configuration.

In your NixOS config:

```nix
security.sudo.wheelNeedsPassword = false;          # for wheel members
# or more precisely:
users.users.yuanw.extraGroups = [ "wheel" ];
```

If you prefer a tighter rule, keep `wheelNeedsPassword = true` but add an
exception for `nixos-rebuild`:

```nix
security.sudo.extraRules = [
  {
    groups = [ "wheel" ];
    commands = [
      { command = "${pkgs.nixos-rebuild}/bin/nixos-rebuild"; options = [ "NOPASSWD" ]; }
    ];
  }
];
```

### Nix daemon with remote access

The remote Nix daemon must support `ssh-ng://` or `ssh://` store URLs. This is
enabled by default if `nix-daemon` is running.

To verify:

```
nix store ping --store ssh://yuanw@target-host
```

## Approach 1: `nixos-rebuild` with `--target-host`

Build the configuration locally, then copy the closure to the remote and
activate it there.

```
nixos-rebuild switch \
  --flake .#hostname \
  --target-host root@target-host
```

**How it works:**

1. Local machine evaluates the flake and builds the system derivation.
2. Nix copies the resulting store paths to the remote via SSH
   (`nix copy --to ssh://...`).
3. `nixos-rebuild switch` runs on the remote to activate the new generation.

**Variations:**

| Flag | Purpose |
|------|---------|
| `--target-host user@host` | Deploy to remote host (default: localhost) |
| `--build-host user@host` | Build on a different host (default: local) |
| `--use-remote-sudo` | Run remote commands via `sudo` instead of as root |

**Example — build locally, deploy to remote as a non-root user with sudo:**

```
nixos-rebuild switch \
  --flake .#dgx-spark \
  --target-host yuanw@dgx-spark \
  --use-remote-sudo
```

**Pros:**

- No build infrastructure needed on the target.
- Build happens on your machine (faster CPU, cached builds from your binary
  cache).
- Simple, no extra tools.

**Cons:**

- The local machine must have the same system architecture, or you need QEMU
  binfmt emulation / remote builders. You cannot build `aarch64-linux` on an
  `x86_64-linux` host without some form of emulation or remote builder.
- Large closure copies can be slow on limited uplinks.

### Cross-architecture builds

If your build machine is `x86_64-linux` and the target is `aarch64-linux`
(like the DGX Spark), you cannot build directly. Options:

1. **Remote builder** — add the target as a remote builder in your
   `/etc/nix/machines` and use `--build-host` (see Approach 2).
2. **QEMU binfmt** — enable QEMU user-mode emulation to build
   `aarch64-linux` on `x86_64-linux` (slow for large derivations).
3. **Build on the target itself** — sync the flake, then run
   `nixos-rebuild` natively on the target.

Option 3 is what the `spark-deploy` just recipe does (see below).

## Approach 2: Build on the remote host

For cross-architecture scenarios or when the target has plenty of RAM and
CPU, sync the flake to the target and build there.

```
rsync -av --exclude=.git --exclude=result ./ user@target:/etc/nixos/
ssh user@target "cd /etc/nixos && sudo nixos-rebuild switch --flake .#hostname"
```

**Pros:**

- Build happens natively on the target — no cross-compilation worries.
- Works across any architecture.
- No QEMU overhead.

**Cons:**

- Slower if the target has limited CPU/RAM.
- Requires the flake source tree on the target.
- Uses the target's Nix cache (which may be cold).

### Build remotely, activate locally (inverse)

You can also build on a powerful remote builder and activate on a different
target:

```
nixos-rebuild switch \
  --flake .#target-host \
  --build-host builder-host \
  --target-host target-host
```

This is useful when you have a dedicated CI build machine.

## Approach 3: `nixos-anywhere` (initial install)

For **first-time provisioning** of a fresh machine (no NixOS installed yet),
use [nixos-anywhere](https://github.com/nix-community/nixos-anywhere). It
bootstraps NixOS over SSH onto a machine running any Linux distribution.

```
nix run github:nix-community/nixos-anywhere -- \
  --flake .#hostname root@<ip>
```

This will:

1. Copy the NixOS installer to the target.
2. Partition the disk using the disko configuration (if present).
3. Install NixOS.
4. Reboot into the new system.

**Prerequisites:**

- Target machine must be booted into *some* Linux (live USB, existing distro)
  with SSH access as root.
- Target must have the disks you intend to use.

**NixOS Anywhere + disko** is the recommended path for headless installs.
See the [DGX Spark readme](../hosts/dgx-spark/readme.org) for a concrete
example.

## Approach 4: `deploy-rs` (multi-host orchestrator)

[deploy-rs](https://github.com/serokell/deploy-rs) is a deployment tool
purpose-built for NixOS flakes. It handles multiple hosts, rollbacks, and
magic rollback on failure.

Add it to your flake:

```nix
inputs.deploy-rs.url = "github:serokell/deploy-rs";
```

Then define deployments:

```nix
deploy.nodes.dgx-spark = {
  hostname = "dgx-spark.local";
  sshUser = "yuanw";
  profiles.system = {
    user = "root";
    path = self.nixosConfigurations.dgx-spark.config.system.build.toplevel;
  };
};
```

Deploy with:

```
nix run .#deploy-rs -- -s .
```

**Pros:**

- Declarative deployment configuration.
- Automatic rollback on activation failure.
- Multi-host in one command.

**Cons:**

- Another tool to learn and integrate.
- Overkill for a single host.

## Approach 5: `colmena` (multi-host orchestrator)

[colmena](https://colmena.cli.rs/) is a deployment tool for NixOS flakes,
similar to deploy-rs but with a different design philosophy. It works with
standard `nixosConfigurations` directly — no separate deployment DSL.

### Basic usage

Add colmena to your flake:

```nix
inputs.colmena.url = "github:zhaofengli/colmena";
```

Add it to your dev shell or call it directly:

```
nix run github:zhaofengli/colmena -- switch --hosts dgx-spark
```

### Configuration

colmena discovers hosts by evaluating all `nixosConfigurations` in your flake
and reading metadata from a `deployment` option:

```nix
{ pkgs, ... }: {
  deployment = {
    # Target hostname / IP
    targetHost = "dgx-spark.local";
    # SSH user (default: root)
    targetUser = "yuanw";
    # Use sudo after connecting (instead of connecting as root)
    buildOnTarget = true;  # build remotely (needed for cross-arch)
  };
}
```

### Deploy commands

| Command | Effect |
|---------|--------|
| `colmena switch` | Build and activate on all hosts |
| `colmena switch --hosts dgx-spark` | Single host |
| `colmena build` | Build only, don't activate |
| `colmena eval` | Evaluate Nix expressions on remote |
| `colmena apply` | Apply a pre-built closure (fast) |

### Cross-architecture with `buildOnTarget`

colmena handles cross-architecture gracefully. Set
`deployment.buildOnTarget = true` in the host config and colmena will:

1. Copy the flake to the target.
2. Build the system derivation natively on the target.
3. Activate the new generation.

This is exactly the rsync + remote build pattern from Approach 2, but
automated by colmena.

### Pros

- Works with standard `nixosConfigurations` — no separate deployment DSL or
  `deploy.nodes` definitions.
- Built-in `buildOnTarget` for cross-architecture deploys.
- Rollback on failure (like deploy-rs).
- Parallel deployment across multiple hosts.
- Can also build locally and copy to target (like `--target-host`).

### Cons

- Another flake input and tool to maintain.
- `buildOnTarget` requires the flake source on the target (rsync overhead).
- Less mature than `nixos-rebuild --target-host` in some edge cases.

### Colmena vs deploy-rs

| Aspect | colmena | deploy-rs |
|--------|---------|-----------|
| Config location | `deployment` option in host NixOS config | Top-level `deploy.nodes` in flake |
| Host discovery | From `nixosConfigurations` | Explicit `deploy.nodes` |
| Cross-arch | `buildOnTarget = true` | Remote builder or separate logic |
| Rollback | Automatic on failure | Automatic on failure |
| Parallel | Yes | Yes (per-node) |
| Flake integration | First-class | First-class |

## Secrets with agenix on remote targets

This repo uses [agenix](https://github.com/ryantm/agenix) for secrets.
Age keys are tied to SSH host keys, so you must ensure the target's SSH
host key is known and its age public key is in `secrets.nix`.

### Age identity on the target

When deploying remotely, the target machine needs its age identity file
(usually `/etc/ssh/ssh_host_ed25519_key` for `ssh-to-age` based setups).

The age identity is automatically converted from the SSH host key if
`agenix.age.ssh-to-age` is enabled (default). Ensure the target's SSH
host key public key is listed in your `secrets/secrets.nix` under the
relevant secret's `publicKeys`.

### Checking age identity remotely

```
ssh target-host "cat /etc/ssh/ssh_host_ed25519_key.pub" | ssh-to-age
```

Then add the output age public key to `secrets.nix` for any secrets that
host needs to decrypt.

### Deploying secrets

When using `--target-host`, agenix secrets are copied as part of the system
closure. The activation script on the target decrypts them using the local
age identity. No extra step is needed — just ensure the target's age key is
in your secrets config.

## This repo's workflow

For this repository, the primary remote deployment targets are:

| Host | Architecture | Deploy method |
|------|-------------|---------------|
| `asche` | `x86_64-linux` | `--target-host`, or local if running on asche |
| `misfit` | `x86_64-linux` | `--target-host`, or local if running on misfit |
| `dgx-spark` | `aarch64-linux` | rsync + build on target (cross-arch) |

### dgx-spark (the common case)

Since the DGX Spark is `aarch64-linux` and the development machine is
`x86_64-linux` or `aarch64-darwin`, the recommended workflow is the
**rsync + remote build** pattern, captured in the `spark-deploy` just
recipe:

```
just spark-deploy IP="dgx-spark.local"
```

This does:

1. `rsync` the flake tree to the target (excluding `.git` and `result`).
2. SSH into the target and run `sudo nixos-rebuild switch --flake .#dgx-spark`.

The flake must already be cloned on the target (one-time setup):

```
ssh yuanw@dgx-spark.local
sudo mkdir -p /etc/nixos
sudo chown yuanw /etc/nixos
git clone https://github.com/yuanw/nix-home.git /etc/nixos
```

### asche / misfit (same-architecture)

For hosts with the same architecture as your build machine (`x86_64-linux`),
you can use `--target-host` directly:

```
nixos-rebuild switch --flake .#asche --target-host root@asche --use-remote-sudo
```

Or, if you're on a macOS build machine, use a remote builder or build
natively on the target.

### Local system (this machine)

To deploy to the machine you're on:

```
nixos-rebuild switch --flake '.#'
```

Or on macOS:

```
darwin-rebuild switch --flake '.'
```

These are aliased as `just switch` in the justfile.

## Troubleshooting

### "error: cannot connect to 'ssh://...'"

- Verify SSH access: `ssh user@host "echo ok"`.
- Check that the remote Nix daemon is running: `systemctl status nix-daemon`.
- Ensure `nix-daemon` listens for SSH connections (it does by default).

### "error: deploying to a remote host without a nix daemon"

The target must have the Nix daemon running. If using `--target-host` with
a non-root user, that user must be in `nix.trusted-users` or `build-users-group`.

### "permission denied" during activation

The remote user needs passwordless sudo for `nixos-rebuild`. Either:

- SSH as `root` directly, or
- Use `--use-remote-sudo` and ensure the SSH user has NOPASSWD sudo.

### Activation succeeds but changes don't take effect

Check the active generation:

```
nixos-rebuild list-generations
```

Compare with the current symlink:

```
readlink /run/current-system
```

### Secrets not decrypting on remote

- Verify the target's age identity file exists: `ls -la /etc/ssh/ssh_host_ed25519_key`.
- Check that the target's age public key is in `secrets.nix`.
- Try decrypting manually on the target: `agenix -d /run/secrets/secret1.age`.
- Ensure `agenix.age.identityPaths` includes the correct path.

### Slow copy over `--target-host`

For large closures, use `nix copy` with compression:

```
nix copy --to ssh://user@host?compression=zstd ./result
```

Or set `nix.extra-options = [ "compress-builds" ];` on the build machine.

### Host key changed warnings

If you rebuild a machine and its SSH host keys change (e.g., after a
reinstall), clean the old key:

```
ssh-keygen -R dgx-spark.local
```

Then ensure the new host key is added to `known_hosts` by verifying the
fingerprint before accepting.
