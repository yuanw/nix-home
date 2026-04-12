# Hashes: nix-prefetch-github thedotmack claude-mem --rev <rev>
{
  pkgs,
  fetchFromGitHub,
  mkClaudePlugin,
  ...
}:
let
  rev = "cde4faae2f33f92d2092ca87537b17b837fdcfb7";
  src = fetchFromGitHub {
    owner = "thedotmack";
    repo = "claude-mem";
    inherit rev;
    hash = "sha256-SkD+QFTPzxjMFZ21XJmKsdroxqeoM4L+xZ3QqlTtWcU=";
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
    version = "12.1.0-unstable-2026-04-09";
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
