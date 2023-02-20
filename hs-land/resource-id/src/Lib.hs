{-# LANGUAGE OverloadedStrings #-}

module Lib (display, decode) where
import           Control.Monad
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as C
import           Data.Char
import           Data.List              (intersperse)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as E
import qualified Data.Text.IO           as TIO
import           System.Console.Pretty  (Color (..), Pretty, color)

type Kind    = B.ByteString
type KeyPath = [(B.ByteString, B.ByteString)]

appendMissingChar :: B.ByteString -> B.ByteString
appendMissingChar input = if modulo == 0 then input else B.append input (C.replicate (4 - modulo) '=')
    where len    = B.length input
          modulo = len `mod` 4

splitBy :: B.ByteString -> [B.ByteString]
splitBy = join . map (C.split (chr 31)) . C.split (chr 30)

decode :: B.ByteString -> Either String KeyPath
decode = (pairUp =<<) . (splitBy <$>) . Base64.decode . appendMissingChar

pairUp :: [B.ByteString] -> Either String KeyPath
pairUp []                 = Right []
pairUp (kind: name: rest) = ((kind, name) :) <$> pairUp rest
pairUp _                  = Left "unmatch kind name"

keyPathToPrettyText :: KeyPath -> T.Text
keyPathToPrettyText paths = color Default "Key(" `T.append`
                            T.intercalate ", "
                            (foldl
                             (\ ls (kind, name) ->
                                ls ++ [color Blue (E.decodeUtf8 kind), color Green (E.decodeUtf8 name)])
                              []
                              paths)
                            `T.append` color Default ")"

displayKeyPath :: KeyPath -> IO ()
displayKeyPath = TIO.putStrLn . keyPathToPrettyText

display :: Either String KeyPath -> IO ()
display (Left err)      = putStrLn (color Red err)
display (Right keyPath) = displayKeyPath keyPath
