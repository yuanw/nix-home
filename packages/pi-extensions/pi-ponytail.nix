# nix-prefetch-github DietrichGebert ponytail --rev <rev>
{
  fetchFromGitHub,
  lib,
  runCommand,
  stdenvNoCC,
  ...
}:
let
  rev = "2ed6c52c9d7e5e56942508591085fd45dea277d3";
  version = "4.9.0-unstable-2026-08-26";

  src = fetchFromGitHub {
    owner = "DietrichGebert";
    repo = "ponytail";
    inherit rev;
    hash = "sha256-bGdXvzhWPwGdz3T2Yh2h6lf+3PBRFAfdBxP5pESmCHI=";
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
