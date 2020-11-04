{ sources ? import ./nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  files = "$(find . -name '*.nix')";
  format = pkgs.writeShellScriptBin "format" "nixpkgs-fmt ${files}";
  lint = pkgs.writeShellScriptBin "lint" "nix-linter ${files} && echo No lint errors!";
  cleanup = pkgs.writeShellScriptBin "cleanup" ''
    set -e
    ${format}/bin/format
  '';
in
pkgs.mkShell {
  buildInputs = [
    pkgs.niv
    pkgs.nixpkgs-fmt
    pkgs.nix-linter
    cleanup
  ];
}
