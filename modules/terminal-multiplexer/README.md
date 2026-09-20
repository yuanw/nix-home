# Tmux Module

Nix module for configuring tmux with opensessions sidebar and which-key menu.

## Options

### Core Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `enable` | bool | `false` | Enable tmux module |
| `mainWorkspaceDir` | string | `"$HOME/workspace"` | Directory for prefix+m shortcut |

### Which-Key Options (`whichKey`)

[tmux-which-key](https://github.com/alexherbo2/tmux-which-key) provides a Spacemacs-style keybinding menu.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `enable` | bool | `false` | Enable which-key menu |
| `prefixKey` | string | `"Space"` | Key to trigger which-key after prefix |

**Usage:**
1. Press `Ctrl+n` (prefix)
2. Press `Space` to show the which-key menu
3. Navigate keybindings visually

**Keybindings:**

| Key | Menu | Description |
|-----|------|-------------|
| `Space` | Run | Open tmux command prompt |
| `Tab` | Last window | Switch to last window |
| `` ` `` | Last pane | Switch to last pane |
| `c` | Copy | Copy mode / List buffers |
| `w` | +Windows | Window management |
| `p` | +Panes | Pane management |
| `b` | +Buffers | Buffer management |
| `s` | +Sessions | Session management |
| `o` | +OpenSessions | Sidebar controls |
| `C` | +Client | Client controls |
| `T` | Time | Clock mode |
| `~` | Messages | Show messages |
| `?` | Keys | List all keys |

### OpenSessions Options (`opensessions`)

[opensessions](https://github.com/Ataraxy-Labs/opensessions) provides a sidebar for tmux session management.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `enable` | bool | `false` | Enable opensessions sidebar |
| `dataDir` | string | `"~/.local/share/opensessions/plugin"` | Directory for opensessions data |
| `key` | string | `""` | Direct toggle key (no prefix, e.g. `"M-s"` for Alt+s) |
| `focusKey` | string | `""` | Direct focus key (no prefix) |
| `width` | int | `26` | Sidebar width in columns |
| `sidebarPosition` | enum | `"left"` | Sidebar position: `"left"` or `"right"` |
| `showWindowDetails` | bool | `true` | Show window/pane details |
| `host` | string | `"127.0.0.1"` | Server host |
| `port` | int | `7391` | Server port |

## Example Configuration

### Basic Setup

```nix
modules.tmux = {
  enable = true;
  mainWorkspaceDir = "$HOME/workspaces";
};
```

### With OpenSessions Sidebar

```nix
modules.tmux = {
  enable = true;
  mainWorkspaceDir = "$HOME/workspaces";
  
  opensessions = {
    enable = true;
    width = 34;
    sidebarPosition = "right";
    showWindowDetails = true;
    
    # Optional: Quick toggle without prefix
    key = "M-s";      # Alt+s to toggle
    focusKey = "M-S"; # Alt+Shift+s to focus
  };
};
```

### Full Setup with Which-Key

```nix
modules.tmux = {
  enable = true;
  mainWorkspaceDir = "$HOME/workspaces";
  
  whichKey = {
    enable = true;
    prefixKey = "Space";
  };
  
  opensessions = {
    enable = true;
    width = 34;
    sidebarPosition = "right";
  };
};
```

## Keybindings

### Default Prefix

All keybindings use `Ctrl+n` as the prefix (configurable via `shortcut = "n"`).

### With Which-Key Enabled

- `Ctrl+n` then `Space` → Opens which-key menu

### With OpenSessions Enabled

**From prefix:**
- `Ctrl+n` then `o` → OpenSessions menu (inside which-key)
- `Ctrl+n` then `O` → Restart opensessions server

**Shell aliases:**
- `osr` → Restart opensessions server
- `osf` → Focus sidebar
- `ost` → Toggle sidebar

**Direct keys (if configured):**
- `Alt+s` → Toggle sidebar (no prefix needed)
- `Alt+Shift+s` → Focus sidebar (no prefix needed)

### Session Management Menu

- `Ctrl+n` then `Tab` → Session menu with options for:
  - Switch sessions
  - Open main workspace
  - Kill sessions
  - Toggle sidebar (if opensessions enabled)

## OpenSessions Menu (within which-key)

When both `whichKey.enable` and `opensessions.enable` are true:

| Key | Action |
|-----|--------|
| `o` → `s` | Focus sidebar |
| `o` → `t` | Toggle sidebar |
| `o` → `1-9` | Switch to session by index |

## Technical Details

### How OpenSessions Works

1. Source code fetched from GitHub and patched to fix hardcoded paths
2. Copied to `dataDir` on first run (version checked via `.version` stamp)
3. `bun install` runs if `node_modules` missing or lock file changed
4. Server starts on `127.0.0.1:7391` (configurable)
5. Sidebar rendered as a tmux pane

### How Which-Key Works

1. YAML config defines keybindings hierarchy
2. Python build script compiles to tmux format
3. Init script loaded from `~/.local/share/tmux/plugins/tmux-which-key/init.tmux`
4. Triggered by `bind-key Space run-shell "cat ..."` after prefix

## Files

```
modules/terminal-multiplexer/
├── default.nix       # Module exports
├── tmux.nix          # Main module definition
├── tmux-which-key.yaml  # Which-key keybindings
├── tat               # Session switcher script
├── ta                # Session attach script
└── README.md         # This file
```

## Dependencies

- `bun` - JavaScript runtime for opensessions
- `curl` - HTTP client for server communication
- `fzf` - Fuzzy finder for session picker
- `python3` - Build dependency for which-key

## Related

- [opensessions](https://github.com/Ataraxy-Labs/opensessions)
- [tmux-which-key](https://github.com/alexherbo2/tmux-which-key)
- [shekohex/dotfiles](https://github.com/shekohex/dotfiles) - Reference implementation