{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    xmonad.url = "github:xmonad/xmonad";
    xmonad-contrib.url = "github:xmonad/xmonad-contrib";
    taffybar.url = "github:taffybar/taffybar";
  };
  outputs = { self, flake-utils, nixpkgs, xmonad, xmonad-contrib, taffybar }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        });
    in
    rec {
      overlay = (final: prev: {
        my-xmobar = final.haskellPackages.callCabal2nix "my-xmobar" ./. { };
      });
      packages =
        forAllSystems (system: { my-xmobar = nixpkgsFor.${system}.my-xmobar; });
      defaultPackage =
        forAllSystems (system: self.packages.${system}.my-xmobar);
      apps.my-xmobar = flake-utils.lib.mkApp { drv = packages.my-xmobar; };
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.my-xmobar ];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            hpack
            cabal-install
          ];
          # Change the prompt to show that you are in a devShell
          shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
        });
    };
}
