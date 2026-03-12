# Hashes: nix-prefetch-github xenodium emacs-skills --rev <rev>
{
  fetchFromGitHub,
  mkClaudePlugin,
  ...
}:
let
  rev = "de7adccbc4aef5f4e1e7ebc7a487bdcd7f95509a";
  src = fetchFromGitHub {
    owner = "xenodium";
    repo = "emacs-skills";
    inherit rev;
    hash = "sha256-ilgWnb3w+6mkeLwy5xkU5iX0NRbguur7iTLVqCu27TA=";
  };
in
mkClaudePlugin {
  pname = "emacs-skills";
  version = "1.0.0";
  inherit rev src;
  marketplace = {
    name = "xenodium";
    inherit src;
    owner = "xenodium";
    repo = "emacs-skills";
  };
}
