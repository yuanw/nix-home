#!/usr/bin/env nix-script-haskell
#!haskellPackages turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

data Command = UpdateDep (Maybe Text) | Rebuild deriving (Show)

-- update one input nix flake lock --update-input nixpkgs
updateDep :: IO ()
updateDep = do
  print "update all inputs"
  shell "nix flake update" empty
  pure ()

rebuild :: IO ()
rebuild = do
  n_ <- hostname
  shell ("nix build .#" <> n_) empty
  print "finish build"

-- main parser
parser :: Parser Command
parser =
  fmap UpdateDep (subcommand "update" "update dep" (optional (argText "name" "name of the input")))
    <|> subcommand "rebuild" "rebuild dotfiles" (pure Rebuild)

-- main
main :: IO ()
main = do
  cmd <- options "the humble helper for your nix home" parser
  case cmd of
    UpdateDep n -> updateDep
    Rebuild -> rebuild
