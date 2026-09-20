# nix-prefetch-github ayghri i-have-adhd --rev <rev>
{
  fetchFromGitHub,
  mkClaudeSkill,
  ...
}:
let
  rev = "cbe69fb83c08a37cf54d5ec9ec6bb88c8bc9973c";
  src = fetchFromGitHub {
    owner = "ayghri";
    repo = "i-have-adhd";
    inherit rev;
    hash = "sha256-56Ia9a8lvALeSmUDAumfu9nzmYBzONSlBpFv7o1w7ys=";
  };
in
mkClaudeSkill {
  pname = "i-have-adhd";
  version = "0.2.0";
  inherit rev src;
  skillMdPath = "skills/i-have-adhd/SKILL.md";
}
