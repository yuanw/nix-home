#!/usr/bin/env nix-script-haskell
#!haskellPackages turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

data Command = UpdateDep (Maybe Text) | Rebuild deriving (Show)

-- main parser
parser :: Parser Command
parser = fmap UpdateDep (subcommand "update" "update dep" (optional (argText "name" "name of the input") ))
    <|> (subcommand "rebuild" "rebuild dotfiles" (pure Rebuild))
-- main
main :: IO ()
main = do
    cmd <- options "the humble helper for your nix home" parser
    case cmd of
      UpdateDep n -> printf ("update dep \n")
      Rebuild -> printf ("rebuild \n")
