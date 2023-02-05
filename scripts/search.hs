#! /usr/bin/env nix-shell
#! nix-shell -p ghcid -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [])" -i runhaskell

main :: IO ()
main = do
  print True
