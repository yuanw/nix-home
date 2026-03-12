# Hashes: nix-prefetch-github blader humanizer --rev <rev>
{
  fetchFromGitHub,
  mkClaudeSkill,
  ...
}:
let
  rev = "c78047bd4300e5a995d37ae8c7684aa2d53326cd";
  src = fetchFromGitHub {
    owner = "blader";
    repo = "humanizer";
    inherit rev;
    hash = "sha256-wkrarl0kHUdfQM5pTMikB/yQm0kngmhsMlqoxZ63Fqs=";
  };
in
mkClaudeSkill {
  pname = "humanizer";
  version = "2.1.1";
  inherit rev src;
}
