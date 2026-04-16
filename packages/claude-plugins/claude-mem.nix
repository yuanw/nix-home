# Hashes: nix-prefetch-github thedotmack claude-mem --rev <rev>
{
  pkgs,
  fetchFromGitHub,
  mkClaudePlugin,
  ...
}:
let
  rev = "70a150db74ca82110a62d194b20aa0422491dd6a";
  src = fetchFromGitHub {
    owner = "thedotmack";
    repo = "claude-mem";
    inherit rev;
    hash = "sha256-QJgxTkZD5cbT9HdzBkNtJj+gbSyPd8UCj+6EwFjLRI0=";
  };

  # claude-mem calls `uvx chroma-mcp [args]` via StdioClientTransport in its
  # bundled binary (not patchable). Intercept by providing a fake uvx that
  # strips uvx-specific flags and runs the named tool from PATH directly.
  fakeUvx = pkgs.writeShellScriptBin "uvx" ''
    args=()
    skip=false
    for arg in "$@"; do
      if $skip; then
        skip=false
        continue
      fi
      case "$arg" in
        --python|--with|--python-preference|--reinstall|--no-cache) skip=true ;;
        --*) ;;
        *) args+=("$arg") ;;
      esac
    done
    exec "''${args[@]}"
  '';
  plugin = mkClaudePlugin {
    pname = "claude-mem";
    version = "12.1.5-unstable-2026-04-15";
    inherit rev src;
    pluginSubdir = "plugin";
    marketplace = {
      name = "thedotmack";
      inherit src;
      owner = "thedotmack";
      repo = "claude-mem";
    };
    runtimeInputs = [
      pkgs.bun
      pkgs.chroma-mcp
    ];
    # Workaround: claude-mem hardcodes 'thedotmack' marketplace path
    activationScript = ''
      ln -sfn "$HOME/.claude/plugins/marketplaces/claude-mem" "$HOME/.claude/plugins/marketplaces/thedotmack"
    '';
  };
in
plugin.overrideAttrs (old: {
  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
  # Prepend fakeUvx before the real uv-provided uvx so claude-mem's bundled
  # binary resolves `uvx chroma-mcp` to pkgs.chroma-mcp without touching the
  # global PATH (avoids conflict with pkgs.uv which also ships uvx).
  postInstall = (old.postInstall or "") + ''
    wrapProgram $out/scripts/claude-mem \
      --prefix PATH : ${
        pkgs.lib.makeBinPath [
          fakeUvx
          pkgs.chroma-mcp
        ]
      }
  '';
})
