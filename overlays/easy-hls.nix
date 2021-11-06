# https://github.com/jkachmar/easy-hls-nix/blob/main/default.nix
{ fetchzip
, installShellFiles
, lib
, lr
, stdenv
  # Optional override for the HLS binaries to support specific GHC versions.
, ghcVersions ? [
    "8.6.4"
    "8.6.5"
    "8.8.3"
    "8.8.4"
    "8.10.2"
    "8.10.3"
    "8.10.4"
    "8.10.5"
    "9.0.1"
  ]
}:
let
  inherit (stdenv) mkDerivation isDarwin;
  hlsBins = [ "wrapper" ] ++ ghcVersions;
in
mkDerivation rec {
  pname = "haskell-language-server";
  version = "1.3.0";
  src =
    if stdenv.isDarwin then
      fetchzip
        {
          url =
            "https://github.com/haskell/haskell-language-server/releases/download/${version}/haskell-language-server-macOS-${version}.tar.gz";
          sha256 = "0mga2m7qx898szfq4w04a3ji7gwh4jm6zasq4xky7c7hwkmr9y5s";
          stripRoot = false;
        }
    else
      fetchzip {
        url =
          "https://github.com/haskell/haskell-language-server/releases/download/${version}/haskell-language-server-Linux-${version}.tar.gz";
        sha256 = "0wgj2jvqfpifcqvyqs4snfln460f4rlgbn66hbmialkg67a2q8qg";
        stripRoot = false;
      };

  nativeBuildInputs = [ installShellFiles ];

  # NOTE: Copied from https://github.com/justinwoo/easy-dhall-nix/blob/master/build.nix
  installPhase = ''
    mkdir -p $out/bin

    ${lib.concatMapStringsSep "\n" (hlsBin: ''
      binPath="$out/bin/haskell-language-server-${hlsBin}"
      # Install HLS
      install -D -m555 -T "haskell-language-server-${hlsBin}" "$binPath"
      rm "haskell-language-server-${hlsBin}"

      # Install bash completions.
      "$binPath" --bash-completion-script "$binPath" > "haskell-language-server-${hlsBin}.bash"
      installShellCompletion --bash "haskell-language-server-${hlsBin}.bash"
      rm "haskell-language-server-${hlsBin}.bash"

      # Install zsh completions.
      "$binPath" --zsh-completion-script "$binPath" > "haskell-language-server-${hlsBin}.zsh"
      installShellCompletion --zsh "haskell-language-server-${hlsBin}.zsh"
      rm "haskell-language-server-${hlsBin}.zsh"

      # Install fish completions.
      "$binPath" --fish-completion-script "$binPath" > "haskell-language-server-${hlsBin}.fish"
      installShellCompletion --fish "haskell-language-server-${hlsBin}.fish"
      rm "haskell-language-server-${hlsBin}.fish"
    '') hlsBins}
  '';
}
