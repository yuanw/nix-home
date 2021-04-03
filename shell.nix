{ sources ? import ./nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  isDarwin = pkgs.stdenvNoCC.isDarwin;
  format = pkgs.writeShellScriptBin "format"
    "${pkgs.fd}/bin/fd --exclude '/nix/sources.nix' -e nix -x ${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
  rebuildDarwin = pkgs.writeShellScriptBin "rebuildDarwin" ''

    darwin-rebuild switch --show-trace \
          -I darwin=${sources.nix-darwin} \
          -I nixpkgs=${sources.nixpkgs}
  '';
  rebuildNix = pkgs.writeShellScriptBin "rebuildNix" ''
    export NIXPKGS_ALLOW_UNFREE=1
    nixos-rebuild switch --show-trace \
          -I nixos-config=./machines/$(hostname)/configuration.nix \
          -I nixpkgs=${sources.nixpkgs}
  '';
  rebuild =
    if pkgs.stdenvNoCC.isDarwin then
      pkgs.writeShellScriptBin "rebuild" ''
        ${format}/bin/format
        ${rebuildDarwin}/bin/rebuildDarwin
      ''
    else
      pkgs.writeShellScriptBin "rebuild" ''
        ${format}/bin/format
        ${rebuildNix}/bin/rebuildNix
      '';

in
pkgs.mkShell {
  name = "dotfiles";
  buildInputs = with pkgs; [ niv rebuild ];
}
