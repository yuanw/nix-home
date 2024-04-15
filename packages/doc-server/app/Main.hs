module Main where

import qualified Data.Foldable as F
import qualified Data.Traversable as T
-- Deprecated: "Use record 'dirTree'"
import System.Directory.Tree

main :: IO ()
main = do
  d <- readDirectoryWithL readFile "."
  mapM_ (putStrLn . name) $ contents $ dirTree d
