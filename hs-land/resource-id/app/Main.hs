module Main where

import Data.ByteString.Char8 qualified as C
import Lib
import Options.Applicative qualified as OA
import System.Environment
import System.Exit

data Config = Config
    { rid :: String
    , decode :: Bool
    }

config :: OA.Parser Config
config = error ""

work :: String -> IO ()
work = display . Lib.decode . C.pack

main :: IO ()
main = do
    args <- getArgs
    if length args == 1 then work (head args) else die "incorrect argument"
