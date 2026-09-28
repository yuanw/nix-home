# ShyFox (vendored)

UserChrome theme for LibreWolf / Firefox. Previously pulled via the `shy-fox`
flake input (`github:Naezr/ShyFox`); now checked in so we can pin a fork that
still works on current Firefox.

## Source

Vendored from [Vortriz/ShyFox](https://github.com/Vortriz/ShyFox) (MPL-2.0).

| | |
|---|---|
| Rev | `b71fc94d394c4fcdec0e9c069e320f26f1adb830` |
| Tip message | fix: repair two long-standing dead selectors (fullscreen state, sidebar position) |
| Date | 2026-09-27 |

Naezr/ShyFox is stale. Vortriz includes the Firefox 156 fix that stopped
`chromehidden` from being set on normal windows — without it, ShyFox hid
`#navigator-toolbox` (no URL bar / toolbar). See
[0bbc41e](https://github.com/Vortriz/ShyFox/commit/0bbc41e1f7a1d06c898d60ac213ef267becf0061).

## Layout

| Path | Role |
|---|---|
| `chrome/` | Profile `chrome` dir (`userChrome.css`, `userContent.css`, icons, wallpapers) |
| `user.js` | Upstream prefs; applied via `profiles.home.settings` in `librewolf-home.nix` (Home Manager writes `user.js` — do not also install this file) |
| `sidebery-settings.json` | Upstream Sidebery export (reference only) |

## How it is wired

- `modules/browsers/librewolf-home.nix` links `chrome/` into the LibreWolf
  profile with `home.file."…/chrome".source = ./shyfox/chrome`.
- ShyFox prefs from `user.js` live in `profiles.home.settings`.
- Active Sidebery config remains `../librewolf-config/sidebery.nix` (custom
  panels). Importing `sidebery-settings.json` wholesale would wipe those.

## Updating

1. Fetch a newer Vortriz (or other) tree.
2. Replace `chrome/`, `user.js`, and `sidebery-settings.json`.
3. Diff `user.js` into `librewolf-home.nix` settings.
4. Update the rev table above.
