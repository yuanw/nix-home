# nix-prefetch-github DietrichGebert ponytail --rev <rev>
{
  fetchFromGitHub,
  lib,
  runCommand,
  stdenvNoCC,
  ...
}:
let
  rev = "9b58c1ffb790c075ca32e70a89cf1d80588f4abf";
  version = "5.1.0-unstable-2026-10-10";

  src = fetchFromGitHub {
    owner = "DietrichGebert";
    repo = "ponytail";
    inherit rev;
    hash = "sha256-S2gHV4/c8XPDqpf0jS8Q5r22//6skZjITf2T9CVhVRE=";
  };

  skillNames = [
    "ponytail"
    "ponytail-audit"
    "ponytail-debt"
    "ponytail-gain"
    "ponytail-help"
    "ponytail-review"
  ];

  mkSkill =
    skillName:
    runCommand "${skillName}-skill"
      {
        pname = skillName;
        inherit version;
        passthru.claudeSkill = {
          pname = skillName;
          inherit version rev;
        };
      }
      ''
        mkdir -p $out
        cp -r ${src}/skills/${skillName}/. $out/
      '';

  skills = lib.genAttrs skillNames mkSkill;
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ponytail";
  inherit version src;

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp ${src}/package.json $out/
    cp -r ${src}/pi-extension $out/
    cp -r ${src}/hooks $out/
    cp -r ${src}/skills $out/
    runHook postInstall
  '';

  passthru = {
    inherit skills rev;
    piExtension = {
      pname = finalAttrs.pname;
      version = finalAttrs.version;
    };
  };

  meta = {
    description = "Lazy senior dev mode for Pi — minimal code, maximum reuse";
    homepage = "https://github.com/DietrichGebert/ponytail";
    license = lib.licenses.mit;
  };
})
