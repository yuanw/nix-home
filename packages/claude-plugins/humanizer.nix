# Hashes: nix-prefetch-github blader humanizer --rev <rev>
{
  pkgs,
  fetchFromGitHub,
  mkClaudePlugin,
  ...
}:
let
  rev = "c78047bd4300e5a995d37ae8c7684aa2d53326cd";
  fetched = fetchFromGitHub {
    owner = "blader";
    repo = "humanizer";
    inherit rev;
    hash = "sha256-wkrarl0kHUdfQM5pTMikB/yQm0kngmhsMlqoxZ63Fqs=";
  };
  # Synthesize the plugin directory structure since the repo is skill-only
  pluginSrc = pkgs.runCommand "humanizer-plugin-src" { } ''
    mkdir -p $out/skills
    touch $out/.claude-plugin
    cp ${fetched}/SKILL.md $out/skills/humanizer.md
  '';
in
mkClaudePlugin {
  pname = "humanizer";
  version = "2.1.1";
  inherit rev;
  src = pluginSrc;
  marketplace = {
    name = "blader";
    src = fetched;
    owner = "blader";
    repo = "humanizer";
  };
}
