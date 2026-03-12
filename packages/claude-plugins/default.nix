{ pkgs, ... }:
let
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

  callPlugin = path: pkgs.callPackage path { inherit mkClaudePlugin; };
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
