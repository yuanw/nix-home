{-# LANGUAGE OverloadedStrings #-}

-- optional
module Main where

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO

level1 :: Text
level1 = "arstneio"

level2 :: Text
level2 = "arstgmneio"

level3 :: Text
level3 = "pbjlarstgmneio"

level4 :: Text
level4 = "pbjlarstgmneiocdvk"

level5 :: Text
level5 = "wfpbjluyarstgmneiocdvk"

main :: IO ()
main = do
  a <- map (head . T.words) . T.lines . E.decodeUtf8 <$> B.readFile "./wordWeights.txt"
  TIO.writeFile "./level1.txt" (T.unwords . filter (T.all (`T.elem` level1)) $ a)
  TIO.writeFile "./level2.txt" (T.unwords . filter (T.all (`T.elem` level2)) $ a)
  TIO.writeFile "./level3.txt" (T.unwords . filter (T.all (`T.elem` level3)) $ a)
  TIO.writeFile "./level4.txt" (T.unwords . filter (T.all (`T.elem` level4)) $ a)
  TIO.writeFile "./level5.txt" (T.unwords . filter (T.all (`T.elem` level5)) $ a)
