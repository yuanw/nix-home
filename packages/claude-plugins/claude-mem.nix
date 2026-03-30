# Hashes: nix-prefetch-github thedotmack claude-mem --rev <rev>
{
  pkgs,
  fetchFromGitHub,
  mkClaudePlugin,
  ...
}:
let
  rev = "d06882126fe24f6ebcbe433385daeb8322ba8009";
  src = fetchFromGitHub {
    owner = "thedotmack";
    repo = "claude-mem";
    inherit rev;
    hash = "sha256-hzG54ODli2KD47OI5w784dPR/D1JMu4oayQ5ovHIhcA=";
  };
in
(mkClaudePlugin {
  pname = "claude-mem";
  version = "10.6.3-unstable-2026-03-29";
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
}).overrideAttrs
  (old: {
    installPhase = old.installPhase + ''
      substituteInPlace $out/scripts/worker-service.cjs \
        --replace-fail 'command:"uvx",args:["--python",r,"chroma-mcp",' \
                       'command:"${pkgs.chroma-mcp}/bin/chroma-mcp",args:['
    '';
  })
