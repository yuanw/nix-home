{ sources ? import ./nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  format = pkgs.writeShellScriptBin "format" "${pkgs.fd}/bin/fd -e nix -x ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
  lint = pkgs.writeShellScriptBin "lint" "${pkgs.fd}/bin/fd -e nix -x ${pkgs.nix-linter}/bin/nix-linter && echo No lint errors!";
  cleanup = pkgs.writeShellScriptBin "cleanup" ''
    set -e
    ${format}/bin/format
  '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    niv
    lint
    cleanup
  ];
}
