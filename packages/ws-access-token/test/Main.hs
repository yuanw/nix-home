module Main where

import Hedgehog
import Hedgehog.Main
import WsAccessToken

main :: IO ()
main = defaultMain [checkParallel $$ discover]
