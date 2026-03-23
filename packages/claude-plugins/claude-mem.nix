# Hashes: nix-prefetch-github thedotmack claude-mem --rev <rev>
{ pkgs
, fetchFromGitHub
, mkClaudePlugin
, ...
}:
let
  rev = "a16b25275e5f56b6d35d5fcf1a8324b8670792c8";
  src = fetchFromGitHub {
    owner = "thedotmack";
    repo = "claude-mem";
    inherit rev;
    hash = "sha256-U1eM3NALFmq6ACYVympRPJMnfW7h9RYdLttW4c9jr04=";
  };
in
(mkClaudePlugin {
  pname = "claude-mem";
  version = "9.0.12";
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
    pkgs.chrome-mcp
  ];
  # Workaround: claude-mem hardcodes 'thedotmack' marketplace path
  activationScript = ''
    ln -sfn "$HOME/.claude/plugins/marketplaces/claude-mem" "$HOME/.claude/plugins/marketplaces/thedotmack"
  '';
}).overrideAttrs (old: {
  installPhase = old.installPhase + ''
    substituteInPlace $out/scripts/worker-service.cjs \
      --replace-fail 'command:"uvx",args:["--python",r,"chroma-mcp",' \
                     'command:"${pkgs.chroma-mcp}/bin/chroma-mcp",args:['
  '';
})
