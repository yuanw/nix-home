{ pkgs, ... }:
let
  # Package a single SKILL.md file into a derivation.
  # The resulting drv exposes `passthru.skillMd` (the source path of SKILL.md)
  # so the home-manager module can link it into ~/.claude/skills/<pname>/SKILL.md.
  mkClaudeSkill =
    {
      pname,
      version,
      rev,
      src,
      # Path of the SKILL.md within src (defaults to root)
      skillMdPath ? "SKILL.md",
    }:
    pkgs.runCommand "${pname}-skill"
      {
        passthru.claudeSkill = {
          inherit pname version rev;
          skillMd = "${src}/${skillMdPath}";
        };
      }
      ''
        mkdir -p $out
        cp ${src}/${skillMdPath} $out/SKILL.md
      '';

  mkClaudePlugin =
    {
      pname,
      version,
      rev,
      src,
      marketplace,
      pluginSubdir ? null,
      runtimeInputs ? [ ],
      activationScript ? "",
    }:
    pkgs.stdenv.mkDerivation {
      inherit pname version src;
      dontBuild = true;
      dontFixup = true;

      installPhase = ''
        mkdir -p $out
        ${if pluginSubdir != null then "cp -r ${pluginSubdir}/. $out/" else "cp -r . $out/"}
      '';

      passthru.claudePlugin = {
        inherit
          pname
          version
          rev
          marketplace
          pluginSubdir
          runtimeInputs
          activationScript
          ;
        id = "${pname}@${marketplace.name}";
      };
    };

  callPlugin = path: pkgs.callPackage path { inherit mkClaudePlugin mkClaudeSkill; };
  claudeCodePlugins = callPlugin ./claude-code-plugins.nix;
in
{
  claude-mem = callPlugin ./claude-mem.nix;
  cozempic = callPlugin ./cozempic.nix;
  humanizer = callPlugin ./humanizer.nix;
  inherit (claudeCodePlugins)
    code-review
    commit-commands
    explanatory-output-style
    frontend-design
    learning-output-style
    pr-review-toolkit
    security-guidance
    ;
}
