---
name: disk-space
description: Find local files and caches that are candidates for deletion to save disk space. Use when the user asks to free space, clean up disk, find large files, audit caches, or prune containers.
allowed-tools: Bash, Read, Grep, Glob
---

# Disk Space Audit

Find deletion candidates on the local machine. **Scan and report only** unless the user
explicitly asks you to delete something.

Scope: $ARGUMENTS (empty = full audit of home + common system caches)

## Rules

1. **Read-only first.** Run scans and size checks before suggesting or running any delete.
2. **Never delete without explicit user approval.** Present candidates grouped by risk.
3. **Show reclaimable size** for every category. Use human-readable units (GB/MB).
4. **Prefer dry-run commands** (`-n`, `--dry-run`, `prune` without `-f`) when available.
5. **Detect OS** (`uname -s`) and branch: macOS vs Linux/NixOS paths differ.
6. **Do not touch** secrets, SSH keys, password stores, active git worktrees the user is
   using, or anything under `~/org`, `~/Documents`, or project source unless the user
   scoped the audit there.

## Workflow

### 1. Disk overview

```bash
df -h
```

Note which filesystem is tight. If the user gave a path, focus scans under that path.

### 2. Parallel size scan of known heavy locations

Run applicable checks in parallel. Skip paths that do not exist.

**macOS**

```bash
# User caches and trash
du -sh ~/.Trash ~/Downloads ~/Library/Caches 2>/dev/null
du -sh ~/Library/Developer/Xcode/DerivedData 2>/dev/null
du -sh ~/Library/Logs 2>/dev/null

# Dev tool caches
du -sh ~/.npm ~/.cache/pip ~/.cache/go-build ~/.cargo/registry ~/.cargo/git 2>/dev/null
du -sh ~/.gradle/caches ~/.m2/repository ~/.cache/yarn ~/.cache/pnpm 2>/dev/null
du -sh ~/.docker ~/.local/share/containers 2>/dev/null   # podman rootless storage

# AI / agent caches
du -sh ~/.pi/agent/cache ~/.pi/agent/sessions 2>/dev/null
du -sh ~/.claude ~/.cursor 2>/dev/null

# Homebrew
brew cleanup -n -s 2>/dev/null || true
```

**Linux / NixOS**

```bash
du -sh ~/.cache ~/.local/share/Trash 2>/dev/null
du -sh ~/.npm ~/.cache/pip ~/.cache/go-build ~/.cargo/registry ~/.cargo/git 2>/dev/null
du -sh ~/.gradle/caches ~/.m2/repository 2>/dev/null
du -sh ~/.local/share/containers ~/.docker 2>/dev/null

# Nix store (often the biggest on NixOS)
nix-store --query --roots /nix/var/nix/profiles 2>/dev/null | wc -l
nix-collect-garbage --dry-run 2>/dev/null || nix-store --gc --print-dead 2>/dev/null | wc -l
du -sh /nix/store 2>/dev/null
```

### 3. Container runtime audit

If `podman` or `docker` is available:

```bash
podman system df 2>/dev/null || docker system df 2>/dev/null
podman images --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}\t{{.ID}}" 2>/dev/null
podman ps -a --format "table {{.Names}}\t{{.Status}}\t{{.Size}}" 2>/dev/null
```

Flag unused images (`podman image prune -a` dry run), stopped containers, and dangling
volumes. Note images tied to running services (e.g. vLLM, isponsorblocktv) as **review**,
not safe.

### 4. Git worktrees and workmux

```bash
workmux list 2>/dev/null || true
find ~/workspaces -maxdepth 3 -type d -name '*__worktrees' 2>/dev/null
```

For each worktree directory found, report total size and whether the branch is merged or
stale. Suggest `workmux rm --gone` or `workmux rm <name>` only after user confirms.

### 5. Large files and old downloads

Find the biggest files under the audit scope (default: home directory, skip `.git`):

```bash
# Top 30 files over 100MB under $HOME (adjust scope if user specified a path)
find "${SCOPE:-$HOME}" -xdev -type f -size +100M \
  ! -path '*/.git/*' ! -path '*/node_modules/*' \
  -exec du -h {} + 2>/dev/null | sort -rh | head -30
```

For Downloads, list files older than 90 days:

```bash
find ~/Downloads -type f -mtime +90 -exec du -ch {} + 2>/dev/null | tail -1
```

### 6. Duplicate and junk patterns (optional, if time permits)

```bash
# Common junk
find "${SCOPE:-$HOME}" -name '.DS_Store' -o -name 'Thumbs.db' 2>/dev/null | wc -l
find "${SCOPE:-$HOME}" -name 'core.*' -o -name '*.log' -size +10M 2>/dev/null | head -20
```

## Report format

Present findings in this structure:

```markdown
## Disk overview
<df summary, highlight tight filesystem>

## Safe to clean (~X GB)
| Location | Size | Command |
|----------|------|---------|
| Homebrew cache | 1.2G | `brew cleanup -s` |
| Podman dangling images | 8.4G | `podman image prune -a` |
| npm cache | 600M | `npm cache clean --force` |

## Review before deleting (~Y GB)
| Location | Size | Notes |
|----------|------|-------|
| ghcr.io/aeon-7/aeon-vllm-ultimate | 12G | Used by vllm service — skip unless service stopped |
| ~/Downloads/foo.iso | 4G | Last modified 2024-01 |

## Large files (~Z GB)
| Path | Size | Suggestion |
|------|------|------------|

## Low priority / small
<items under ~50MB unless user asked for exhaustive scan>

## Suggested next step
Ask which categories to clean. Run one category at a time and re-check `df -h` after.
```

### Risk categories

| Category | Examples | Default action |
|----------|----------|----------------|
| **Safe** | Package caches, dangling images, brew cleanup, trash | Suggest cleanup commands |
| **Review** | Unused images, old downloads, merged worktrees, log archives | List with context; wait for OK |
| **Risky** | Nix GC, podman volumes, model weights, anything in active projects | Warn explicitly; never auto-delete |

## Cleanup commands reference

Only run these **after user confirms** the category:

```bash
# Containers
podman image prune -a -f
podman system prune -a -f              # + stopped containers
podman system prune -a --volumes -f  # + unused volumes (risky)

# Package managers
brew cleanup -s
npm cache clean --force
pip cache purge
cargo cache -a
rm -rf ~/.gradle/caches/*
nix-collect-garbage -d                 # NixOS only; review first

# Trash
rm -rf ~/.Trash/*                      # macOS
rm -rf ~/.local/share/Trash/*          # Linux

# Worktrees (merged / gone remote only)
workmux rm --gone
workmux rm <handle>
```

## Scope shortcuts

When `$ARGUMENTS` is set, narrow the audit:

| Argument | Behavior |
|----------|----------|
| `podman` / `containers` | Section 3 only |
| `caches` | Section 2 dev caches only |
| `downloads` | ~/Downloads + large old files |
| `worktrees` | Section 4 only |
| `nix` | Nix store / GC candidates only |
| `/path/to/dir` | Scan that directory tree |
| `aggressive` | Include review items and lower the large-file threshold to 50MB |
