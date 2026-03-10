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
in
{
  claude-mem = callPlugin ./claude-mem.nix;
}
