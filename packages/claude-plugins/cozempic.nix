# Hashes: nix-prefetch-github Ruya-AI cozempic --rev <rev>
{
  pkgs,
  fetchFromGitHub,
  mkClaudePlugin,
  ...
}:
let
  rev = "63ce69a42576c81ffde5bee86cfe808339d889be";
  src = fetchFromGitHub {
    owner = "Ruya-AI";
    repo = "cozempic";
    inherit rev;
    hash = "sha256-hBjBFrnETrRoDaAs7FBp/P9VMi3xvMjtgjSpDwKRLkE=";
  };
in
mkClaudePlugin {
  pname = "cozempic";
  version = "0.9.0";
  inherit rev src;
  pluginSubdir = "plugin";
  marketplace = {
    name = "Ruya-AI";
    inherit src;
    owner = "Ruya-AI";
    repo = "cozempic";
  };
  runtimeInputs = [
    pkgs.uv
  ];
}
