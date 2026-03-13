# Hashes: nix-prefetch-github jwiegley claude-prompts --rev <rev>
{ fetchFromGitHub, mkClaudeSkill, ... }:
let
  rev = "39475306a3462d1ecb4697135b24cfaf6184409c";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "claude-prompts";
    inherit rev;
    hash = "sha256-AggJ0MAHvUX72xxMeeXZr4h6lmekmyLryrtplI/Am+w=";
  };
in
mkClaudeSkill {
  pname = "caveman";
  version = "0-unstable-2026-03-11";
  inherit rev src;
  skillMdPath = "skills/caveman/SKILL.md";
}
