{ sources ? import ./nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  format = pkgs.writeShellScriptBin "format"
    "${pkgs.fd}/bin/fd --exclude '/nix/sources.nix' -e nix -x ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
  lint = pkgs.writeShellScriptBin "lint"
    "${pkgs.fd}/bin/fd --exclude '/nix/sources.nix' -e nix -x ${pkgs.nix-linter}/bin/nix-linter && echo No lint errors!";
  rebuildDarwin = pkgs.writeShellScriptBin "rebuildDarin" ''
    darwin-rebuild switch --show-trace \
          -I darwin=${sources.nix-darwin} \
          -I nixpkgs=${sources.nixpkgs}
  '';
  rebuildNix = pkgs.writeShellScriptBin "rebuildNix" ''
    darwin-rebuild switch --show-trace \
          -I nixos-config=machines/$(hostname).nix
          -I nixpkgs=${sources.nixpkgs}
  '';
  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    ${format}/bin/format
    ${lint}/bin/lint
    ${if pkgs.stdenvNoCC.isDarwin then "rebuildDarwin" else "rebuildNix"}
  '';
in
pkgs.mkShell {
  name = "dotfiles";
  buildInputs = with pkgs; [ niv rebuild ];
}
