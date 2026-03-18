# Hashes: nix-prefetch-github anthropics claude-code --rev <rev>
{ fetchFromGitHub, mkClaudePlugin, ... }:
let
  rev = "84d7b085394b687e1531784350a858918fa6f8ed";
  src = fetchFromGitHub {
    owner = "anthropics";
    repo = "claude-code";
    inherit rev;
    hash = "sha256-2L/sAlKEENb+SpBrgtxPCka8LVhdWAqOOTzikvSHv08=";
  };
  marketplace = {
    name = "claude-code-plugins";
    inherit src;
    owner = "anthropics";
    repo = "claude-code";
  };
  mkPlugin =
    name:
    mkClaudePlugin {
      pname = name;
      version = "1.0.0";
      inherit rev src marketplace;
      pluginSubdir = "plugins/${name}";
    };
in
{
  code-review = mkPlugin "code-review";
  commit-commands = mkPlugin "commit-commands";
  explanatory-output-style = mkPlugin "explanatory-output-style";
  frontend-design = mkPlugin "frontend-design";
  learning-output-style = mkPlugin "learning-output-style";
  pr-review-toolkit = mkPlugin "pr-review-toolkit";
  security-guidance = mkPlugin "security-guidance";
}
